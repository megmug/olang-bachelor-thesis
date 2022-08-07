{-# OPTIONS_GHC -Wno-type-defaults #-}

module Machine where

import Control.Lens (set, use, view, (+=), (.=))
import Control.Monad (replicateM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, get, runState)
import qualified Data.IntMap as M
import qualified Data.Vector as V
import MachineInstruction
  ( BinaryOperator (..),
    ClassID,
    CodeAddress,
    Instruction (..),
    MethodID,
    UnaryOperator (..),
  )

{-# ANN module ("hlint: ignore Avoid lambda") #-}

{- Type definitions -}
data Machine
  = Machine
      Code
      Stack
      InstructionRegister
      ProgramCounter
      BaseAddressRegister
      ObjectCounter
      Heap
      MethodTables
      InputBuffer
      OutputBuffer
  deriving (Eq)

type Code = V.Vector Instruction

type Stack = V.Vector Integer

type InstructionRegister = Instruction

type ProgramCounter = Int

type BaseAddressRegister = Int

type ObjectCounter = Int

type Heap = M.IntMap Object

data Object = OBJ ClassID (V.Vector Integer) deriving (Eq, Show)

type MethodTables = [(ClassID, MethodTable)]

type MethodTable = [(MethodID, CodeAddress)]

type InputBuffer = [String]

type OutputBuffer = [String]

-- A computation is a monadic action featuring a Machine state and a possible String exception
-- The result might be a string, but it doesn't have to be
type Computation a = ExceptT String (State Machine) a

{--}

{- Define lenses for our machine data type -}
code :: Functor f => (Code -> f Code) -> Machine -> f Machine
code f (Machine c s i pc b o h mts input output) = (\c' -> Machine c' s i pc b o h mts input output) <$> f c

stack :: Functor f => (Stack -> f Stack) -> Machine -> f Machine
stack f (Machine c s i pc b o h mts input output) = (\s' -> Machine c s' i pc b o h mts input output) <$> f s

iregister :: Functor f => (InstructionRegister -> f InstructionRegister) -> Machine -> f Machine
iregister f (Machine c s i pc b o h mts input output) = (\i' -> Machine c s i' pc b o h mts input output) <$> f i

programcounter :: Functor f => (ProgramCounter -> f ProgramCounter) -> Machine -> f Machine
programcounter f (Machine c s i pc b o h mts input output) = (\pc' -> Machine c s i pc' b o h mts input output) <$> f pc

bregister :: Functor f => (BaseAddressRegister -> f BaseAddressRegister) -> Machine -> f Machine
bregister f (Machine c s i pc b o h mts input output) = (\b' -> Machine c s i pc b' o h mts input output) <$> f b

ocounter :: Functor f => (ObjectCounter -> f ObjectCounter) -> Machine -> f Machine
ocounter f (Machine c s i pc b o h mts input output) = (\o' -> Machine c s i pc b o' h mts input output) <$> f o

heap :: Functor f => (Heap -> f Heap) -> Machine -> f Machine
heap f (Machine c s i pc b o h mts input output) = (\h' -> Machine c s i pc b o h' mts input output) <$> f h

mtables :: Functor f => (MethodTables -> f MethodTables) -> Machine -> f Machine
mtables f (Machine c s i pc b o h mts input output) = (\mts' -> Machine c s i pc b o h mts' input output) <$> f mts

inbuffer :: Functor f => (InputBuffer -> f InputBuffer) -> Machine -> f Machine
inbuffer f (Machine c s i pc b o h mts input output) = (\input' -> Machine c s i pc b o h mts input' output) <$> f input

outbuffer :: Functor f => (OutputBuffer -> f OutputBuffer) -> Machine -> f Machine
outbuffer f (Machine c s i pc b o h mts input output) = (\output' -> Machine c s i pc b o h mts input output') <$> f output

{--}

{- Utility functions -}
instance Show Machine where
  show (Machine _ s i pc b o h mts input output) =
    "Machine state:"
      ++ "\nStack: "
      ++ show s
      ++ "\nInstruction register: "
      ++ show i
      ++ "\nProgram counter: "
      ++ show pc
      ++ "\nBase address register: "
      ++ show b
      ++ "\nObject counter: "
      ++ show o
      ++ "\nHeap: "
      ++ show h
      ++ "\nMethod tables: "
      ++ show mts
      ++ "\nInput stream: "
      ++ show input
      ++ "\nOutput stream: "
      ++ show output

{- Utility code for displaying machine traces in latex tables for the bachelor thesis paper -}
data LatexShowMode = CORE | PROC | FULL

showLatexTableRow :: LatexShowMode -> Int -> Machine -> String
showLatexTableRow CORE stepnum (Machine _ s i pc _ _ _ _ _ _) = show stepnum ++ " & " ++ show pc ++ " & \\haskell{" ++ show i ++ "} & \\haskell{" ++ show s ++ "} " ++ "\\tabularnewline \\hline"
showLatexTableRow PROC stepnum (Machine _ s i pc b _ _ _ _ _) = show stepnum ++ " & " ++ show pc ++ " & \\haskell{" ++ show i ++ "} & \\haskell{" ++ show s ++ "} & " ++ show b ++ " \\tabularnewline \\hline"
showLatexTableRow FULL stepnum (Machine _ s i pc b oc h mt _ _) = show stepnum ++ " & " ++ show pc ++ " & \\haskell{" ++ show i ++ "} & \\haskell{" ++ show s ++ "} & " ++ show b ++ " & \\haskell{" ++ show mt ++ "} & \\haskell{" ++ drop 9 (show h) ++ "} & " ++ show oc ++ " \\tabularnewline \\hline"

-- Show a code segment with position markings
customShow :: Code -> String
customShow cs = "[" ++ showElements 0 cs ++ "]"
  where
    showElements i cs'
      | V.null cs' = ""
      | i /= 0 = ", " ++ show i ++ ": " ++ show (V.head cs') ++ showElements (i + 1) (V.tail cs')
      | otherwise = show i ++ ": " ++ show (V.head cs') ++ showElements (i + 1) (V.tail cs')

isIndexForVector :: Int -> V.Vector a -> Bool
isIndexForVector i v = 0 <= i && i < V.length v

isHalted :: Machine -> Bool
isHalted m = case view iregister m of
  Halt -> True
  _ -> False

integerToBool :: Integer -> Maybe Bool
integerToBool 0 = Just False
integerToBool 1 = Just True
integerToBool _ = Nothing

boolToInteger :: Bool -> Integer
boolToInteger False = 0
boolToInteger True = 1

createMachine :: [Instruction] -> Maybe Machine
createMachine cs = createMachineWithInput cs []

createMachineWithInput :: [Instruction] -> InputBuffer -> Maybe Machine
createMachineWithInput [] _ = Nothing
createMachineWithInput (c : cs) inputs = Just $ Machine (V.fromList (c : cs)) (V.fromList [0, 0]) c 1 0 0 M.empty [] inputs []

combineUnary :: UnaryOperator -> Integer -> Maybe Integer
combineUnary Not 0 = Just 1
combineUnary Not 1 = Just 0
combineUnary _ _ = Nothing

combineBinary :: BinaryOperator -> Integer -> Integer -> Integer
combineBinary op n m = case op of
  Equals -> boolToInteger $ n == m
  Smaller -> boolToInteger $ n < m
  Greater -> boolToInteger $ n > m
  Plus -> n + m
  Minus -> n - m
  Times -> n * m
  Divide -> n `div` m

{--}

{- Commonly used state computations -}
throwDiagnosticError :: String -> Computation a
throwDiagnosticError e = do
  m <- lift get
  throwE $ e ++ "\n" ++ show m

controlComm :: String -> Computation ()
controlComm e = do
  throwE $ "CONTROL:" ++ e

loadInstruction :: CodeAddress -> Computation ()
loadInstruction a = do
  prog <- use code
  if isIndexForVector a prog
    then do
      iregister .= prog V.! a
      programcounter .= a + 1
    else throwDiagnosticError "code address out of range"

push :: Integer -> Computation ()
push n = do
  s <- use stack
  stack .= V.snoc s n

pop :: Computation Integer
pop = do
  s <- use stack
  case V.unsnoc s of
    Nothing -> throwDiagnosticError "stack is empty on pop"
    Just (bottom, top) -> do
      stack .= bottom
      return top

popFrame :: Computation ()
popFrame = do
  s <- use stack
  b <- use bregister
  bNew <- stackGet b
  stack .= V.take b s
  bregister .= fromInteger bNew

stackGet :: Int -> Computation Integer
stackGet a = do
  s <- use stack
  if isIndexForVector a s
    then return (s V.! a)
    else throwDiagnosticError "stack address out of range!"

stackSet :: Int -> Integer -> Computation ()
stackSet a e = do
  s <- use stack
  if isIndexForVector a s
    then stack .= V.update s (V.fromList [(a, e)])
    else throwDiagnosticError "stack address out of range!"

heapGetObj :: Int -> Computation Object
heapGetObj a = do
  h <- use heap
  case M.lookup a h of
    Nothing -> throwDiagnosticError "heap address out of range!"
    Just o -> return o

heapSetObj :: Int -> Object -> Computation ()
heapSetObj a o = do
  h <- use heap
  heap .= M.insert a o h

heapSetField :: Int -> Int -> Integer -> Computation ()
heapSetField a i val = do
  (OBJ cid fs) <- heapGetObj a
  if isIndexForVector i fs
    then heapSetObj a (OBJ cid (V.update fs (V.fromList [(i, val)])))
    else throwDiagnosticError "field index out of range!"

lookupMethod :: Int -> Int -> Computation CodeAddress
lookupMethod cid mid = do
  mtt <- use mtables
  case lookup cid mtt of
    Nothing -> throwDiagnosticError "referenced invalid class"
    Just mt -> case lookup mid mt of
      Nothing -> throwDiagnosticError "referenced invalid method"
      Just a -> return a

{--}

{- Core stepper computation
 - This is independent from a concrete computational context like IO to keep things modular
 - Through the environment, the computation will take in inputs through its input buffer and output messages through its output buffer
 -}
step :: Computation ()
step = do
  currentInstruction <- use iregister
  case currentInstruction of
    {-- Core machine instructions --}
    {- PushInt n: Push integer n onto the stack
       Effect:
        push n
        loadNextInstruction
    -}
    PushInt n -> do
      push n
      pc <- use programcounter
      loadInstruction pc
    {- LoadStack a: Load the value from stack address a (relative to base address) and push it onto the stack
       Effect:
        push stack[B + 2 + a]
        loadNextInstruction
    -}
    LoadStack a -> do
      b <- use bregister
      e <- stackGet $ b + 2 + a -- calculate actual index by accounting for base address, and stack frame information
      push e
      pc <- use programcounter
      loadInstruction pc
    {- StoreStack a: Pop the stack's topmost value and store it to stack address a
       Effect:
        stack[B + 2 + a] := pop
        loadNextInstruction
    -}
    StoreStack a -> do
      b <- use bregister
      top <- pop
      stackSet (b + 2 + a) top
      pc <- use programcounter
      loadInstruction pc
    {- CombineUnary op: Combine the stack's topmost value with operator op
       Effect:
        push (op pop)
        loadNextInstruction
    -}
    CombineUnary Not -> do
      top <- pop
      case combineUnary Not top of
        Nothing -> throwDiagnosticError "CombineUnary: input value is not a boolean"
        Just n -> do
          push n
          pc <- use programcounter
          loadInstruction pc
    {- CombineBinary op: Combine the stack's two topmost values using operator op
       Effect:
        snd := pop
        fst := pop
        push (op fst snd)
        loadNextInstruction
    -}
    CombineBinary op -> do
      sndEl <- pop
      fstEl <- pop
      push $ combineBinary op fstEl sndEl
      pc <- use programcounter
      loadInstruction pc
    {- Jump a: Unconditionally jump to code address a
       Effect: loadInstruction a
    -}
    Jump a -> loadInstruction a
    {- JumpIfFalse a: Jump to code address a if the stack's topmost value represents a boolean value of False
       Effect:
        if pop = 0
        then loadInstruction a
        else loadNextInstruction
    -}
    JumpIfFalse a -> do
      top <- pop
      case integerToBool top of
        Nothing -> throwDiagnosticError "JumpIfFalse: condition was not boolean!"
        Just False -> loadInstruction a
        Just True -> do
          pc <- use programcounter
          loadInstruction pc
    {- Read: Read an integer value from the environment's input and push it onto the stack
       Effect:
        push read
        loadNextInstruction
    -}
    Read -> do
      inputs <- use inbuffer
      when (null inputs) $ controlComm "IN"
      inbuffer .= tail inputs
      push (read $ head inputs :: Integer)
      pc <- use programcounter
      loadInstruction pc
    {- PrintInt: Pop the stack's topmost value and print it to the environment's output
       Effect:
        print pop
        loadNextInstruction
    -}
    PrintInt -> do
      toWrite <- pop
      out <- use outbuffer
      outbuffer .= out ++ [show toWrite]
      pc <- use programcounter
      loadInstruction pc
      controlComm "OUT"
    {- PrintStr s: Print s to the environment's output
       Effect:
        print s
        loadNextInstruction
    -}
    PrintStr msg -> do
      out <- use outbuffer
      outbuffer .= out ++ [msg]
      pc <- use programcounter
      loadInstruction pc
      controlComm "OUT"
    {- PrintStrLn s: Print s followed by a new line character to the environment's output
       Effect:
        print (s ++ '\n')
        loadNextInstruction
    -}
    PrintStrLn msg -> do
      out <- use outbuffer
      outbuffer .= out ++ [msg ++ "\n"]
      pc <- use programcounter
      loadInstruction pc
      controlComm "OUT"
    {- Halt: Halt the machine
       Effect:
        nop
    -}
    Halt -> return ()
    {-- Instructions for procedure support --}
    {- Invoke the procedure at code address a, creating a new stack frame and passing n parameters
       Effect:
        p_n := pop
        ...
        p_1 := pop
        push B
        B := length stack - 1
        push PC
        push p_1
        ...
        push p_n
        loadInstruction a
    -}
    CallProcedure a n -> do
      inverseParams <- replicateM n pop
      b <- use bregister
      pc <- use programcounter
      push $ toInteger b
      s <- use stack
      bregister .= V.length s - 1
      push $ toInteger pc
      mapM_ push (reverse inverseParams)
      loadInstruction a
    {- Return ret: Return from the current procedure invocation, jumping back to the return address, popping the current stack frame, and, depending on ret, possibly returning a value
       Effect:
        BOld := B
        ra := stack[B + 1]

        if ret == True
        then retVal := pop

        B := stack[B]
        while length stack > BOld
        do pop

        if ret == True
        then push retVal

        loadInstruction ra
    -}
    Return ret -> do
      b <- use bregister
      ra <- stackGet $ b + 1
      retVal <-
        if ret
          then pop
          else return 0
      popFrame
      when ret $ push retVal
      loadInstruction $ fromInteger ra

    {-- Instructions for support of object-oriented features --}
    {- LoadHeap i: Push to the stack the field value of the object with address \texttt{a} and field index \texttt{i}.
       Effect:
        a := pop
        obj := H[a]
        push fields(obj)[i]
        loadNextInstruction
    -}
    LoadHeap i -> do
      a <- pop
      (OBJ _ fs) <- heapGetObj $ fromInteger a
      if isIndexForVector i fs
        then do
          push $ fs V.! i
          pc <- use programcounter
          loadInstruction pc
        else throwDiagnosticError "LoadHeap: referenced a non-existing field!"
    {- StoreHeap i: Store the stack's topmost value to the field with index i, of object with address a where a is the stack's second topmost value
       Effect:
        val := pop
        a := pop
        fields(H[a])[i] := val
        loadNextInstruction
    -}
    StoreHeap i -> do
      val <- pop
      a <- pop
      heapSetField (fromInteger a) i val
      pc <- use programcounter
      loadInstruction pc
    {- AllocateHeap n t: Create on the heap a new object with \texttt{n} fields and class identifier \texttt{cid} and push object's address to the stack.
       Effect:
        H[O] := createObj(n, cid)
        push O
        O := O + 1
        loadNextInstruction
    -}
    AllocateHeap n cid -> do
      mt <- use mtables
      case lookup cid mt of
        Nothing -> throwDiagnosticError "AllocateHeap: referenced a non-existing class"
        Just _ -> do
          o <- use ocounter
          heapSetObj o $ OBJ cid (V.fromList (replicate n 0))
          push $ toInteger o
          ocounter += 1
          pc <- use programcounter
          loadInstruction pc
    {- CreateMethodTable id [(0, a0), (1, a1), ..., (n, an)]: Create a new method table with class identifier \texttt{cid} that holds the code addresses \texttt{a0 ... an} for methods with identifiers \texttt{id0 ... idn}
       Effect:
        MTT[cid] := [(id0, a0), ..., (idn, an)]
        loadNextInstruction
    -}
    CreateMethodTable cid methods -> do
      mtt <- use mtables
      case lookup cid mtt of
        Just _ -> throwDiagnosticError "CreateMethodTable: method table already exists"
        Nothing -> do
          mtables .= (cid, methods) : mtt
          pc <- use programcounter
          loadInstruction pc
    {- CallMethod mid n invokes method with the stack's n + 1 topmost values as parameters, of which the first is the address of the parameter object
       The method invoked is the one with index i in the corresponding method table
       Effect:
        pn := pop
        ...
        p1 := pop
        oa := pop
        push B
        B := length stack - 1
        push PC
        push oa
        push p1
        ...
        push pn
        obj := H[oa]
        cid := classid(obj)
        loadInstruction MTT[cid][mid]
    -}
    CallMethod mid n -> do
      inverseParams <- replicateM n pop
      oa <- pop
      b <- use bregister
      push $ toInteger b
      s <- use stack
      bregister .= V.length s - 1
      pc <- use programcounter
      push $ toInteger pc
      push oa
      mapM_ push (reverse inverseParams)
      (OBJ cid _) <- heapGetObj $ fromInteger oa
      a <- lookupMethod cid mid
      loadInstruction a

run :: Computation ()
run = do
  -- while I != Halt do executeInstruction
  step
  i <- use iregister
  when (i /= Halt) $ do
    run

{--}

{- Machine environment/runner implementations for concrete computational contexts -}
{- For a sensible runner implementation, the instruction cycle can be described by
    code := program
    I := code[0]
    PC := 1
    stack := [0, 0]
    B := 0
    H := []
    O := 0
    MTT := []
    while I != Halt do executeInstruction
 -}
runProgram :: [Instruction] -> IO ()
runProgram = runProgramIO

runProgramTrace :: [Instruction] -> IO ()
runProgramTrace = runTraceIO

runProgramTest :: [Instruction] -> InputBuffer -> Either String String
runProgramTest cs s = case createMachineWithInput cs s of
  Nothing -> Left "invalid machine code"
  Just m -> runTest m

runProgramIO :: [Instruction] -> IO ()
runProgramIO cs = case createMachine cs of
  Nothing -> error "invalid machine code"
  Just m -> runIO m

runIO :: Machine -> IO ()
runIO m = case runState (runExceptT run) m of
    (Right (), _) -> return ()
    (Left "CONTROL:IN", m') -> do
      l <- getLine
      runIO $ set inbuffer [l] m'
    (Left "CONTROL:OUT", m') -> do
      let out = view outbuffer m'
      putStr $ concat out
      runIO (set outbuffer [] m')
    (Left e, _) -> error e

runTest :: Machine -> Either String String
runTest m = case runState (runExceptT run) m of
  (Right (), m') -> Right $ concat $ view outbuffer m'
  (Left "CONTROL:OUT", m') -> runTest m'
  (Left e, _) -> error e

runTraceIO :: [Instruction] -> IO ()
runTraceIO cs = case createMachine cs of
    Nothing -> error "invalid machine code"
    Just m -> stepIOWithTraceUntilHalted m 0 False FULL
  where
    stepIOWithTraceUntilHalted m n generateLatex latexLevel = do
      printInfo m n generateLatex latexLevel
      m' <- stepIO m
      if isHalted m'
        then do
          printInfo m' (n + 1) generateLatex latexLevel
        else do
          when generateLatex $ putStrLn $ showLatexTableRow FULL (n + 1) m'
          stepIOWithTraceUntilHalted m' (n + 1) generateLatex latexLevel

    printInfo m stepNum True l = putStrLn $ showLatexTableRow l stepNum m
    printInfo m stepNum False _ = do
          putStrLn ("Machine runtime: " ++ show stepNum)
          print m
          putStrLn ""

stepIO :: Machine -> IO Machine
stepIO m = case runState (runExceptT step) m of
    (Left "CONTROL:IN", m') -> do
      l <- getLine
      let m'' = set inbuffer [l] m'
      return m''
    (Left "CONTROL:OUT", m') -> do
      putStr $ concat $ view outbuffer m'
      return $ set outbuffer [] m'
    (Left e, _) -> error e
    (Right (), m') -> return m'

{--}