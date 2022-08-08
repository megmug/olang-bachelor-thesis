{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CodeGenerator where

import Control.Lens (use, view, (%=), (+=), (.=))
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.State (State, evalState)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import MachineInstruction
  ( BinaryOperator
      ( Divide,
        Equals,
        Greater,
        Minus,
        Plus,
        Smaller,
        Times
      ),
    ClassID,
    CodeAddress,
    Instruction
      ( AllocateHeap,
        CallMethod,
        CallProcedure,
        CombineBinary,
        CombineUnary,
        CreateMethodTable,
        Halt,
        Jump,
        JumpIfFalse,
        LoadHeap,
        LoadStack,
        PrintInt,
        PrintStr,
        PrintStrLn,
        PushInt,
        Read,
        Return,
        StoreHeap,
        StoreStack
      ),
    MethodID,
    UnaryOperator (Not),
  )
import SyntaxTree
  ( Call (..),
    ClassDeclaration (..),
    ClassName,
    Condition (..),
    Expression (..),
    Factor (..),
    FormalParameterList,
    Instruction (..),
    IntSymbolDeclaration (Int),
    MethodDeclaration (..),
    ObjectSymbolDeclaration (Object),
    Operator (Divide, Times),
    ProcedureDeclaration (..),
    ProcedureHeader (ProcedureHeader),
    Program (..),
    Relation (Equals, Greater, Smaller),
    Sign (Minus, Plus),
    SymbolDeclaration (IntDeclaration, ObjectDeclaration),
    SymbolReference (FieldReference, NameReference),
    Term (..),
  )

{-# ANN module ("hlint: ignore Avoid lambda") #-}

{- Basic helper type definitions -}
-- A symbol has a name, a type as well as a position in the local variable segment on the stack
data SymbolEntry = SymbolEntry String Type Position deriving (Eq, Show)

-- A type can be either a normal primitive integer or an object with a type
data Type = INT | OBJ String deriving (Eq, Show)

-- An optional type is what occurs in an expression hole, since it can be a procedure/method call with no return value
type OptionalType = ReturnType

-- A return type is either empty or a normal type
type ReturnType = Maybe Type

type Position = Int

-- symbol tables are used like a stack whose top is on the front
-- a new symbol on top will shadow old symbols with the same type
type SymbolTable = [SymbolEntry]

-- A procedure has a signature and an address in the program
data ProcedureEntry = ProcedureEntry Signature CodeAddress deriving (Eq, Show)

-- A procedure is either a standard procedure or an initializer/constructor
data ProcedureKind = NORMAL | INIT deriving (Eq, Show)

-- A signature consists a name, a list that resembles the parameter types and a return type
data Signature = Signature String [Type] ReturnType deriving (Eq, Show)

type ProcedureTable = [ProcedureEntry]

type ClassTable = [(ClassID, ClassEntry)]

-- A class has a name, possibly an upper class, a table of fields and a table of methods
data ClassEntry = ClassEntry String (Maybe ClassID) FieldTable MethodTable deriving (Eq, Show)

type FieldTable = [FieldEntry]

-- A field has a name, a type and a position in the heap frame
data FieldEntry = FieldEntry String Type Position deriving (Eq, Show)

type MethodTable = [(MethodID, ProcedureEntry)]

type PrefixLength = Int -- Denotes the length of the preceding program at certain point

{- The code generator maintains a state that consists of a current prefix
 - as well as a symbol table
 - a procedure table
 - and a class table
 -}
data GenState = GenState PrefixLength SymbolTable ProcedureTable ClassTable

-- The typifier essentially shares the code generator's state, but doesn't need to know the current prefix
data TypeState = TypeState SymbolTable ProcedureTable ClassTable

-- Here we define a type for monadic actions that represent the types of our code generators and typifiers
type Generator a = ExceptT String (State GenState) a

-- A typifier only needs to read the state, hence the use of Reader instead of State
type Typifier a = ExceptT String (Reader TypeState) a

{--}

{- Lens definitions for GenState and TypeState -}
prefixLength :: Functor f => (PrefixLength -> f PrefixLength) -> GenState -> f GenState
prefixLength f (GenState pl st pt ct) = (\pl' -> GenState pl' st pt ct) <$> f pl

symbolTable :: Functor f => (SymbolTable -> f SymbolTable) -> GenState -> f GenState
symbolTable f (GenState pl st pt ct) = (\st' -> GenState pl st' pt ct) <$> f st

procedureTable :: Functor f => (ProcedureTable -> f ProcedureTable) -> GenState -> f GenState
procedureTable f (GenState pl st pt ct) = (\pt' -> GenState pl st pt' ct) <$> f pt

classTable :: Functor f => (ClassTable -> f ClassTable) -> GenState -> f GenState
classTable f (GenState pl st pt ct) = (\ct' -> GenState pl st pt ct') <$> f ct

symbolTableT :: Functor f => (SymbolTable -> f SymbolTable) -> TypeState -> f TypeState
symbolTableT f (TypeState st pt ct) = (\st' -> TypeState st' pt ct) <$> f st

procedureTableT :: Functor f => (ProcedureTable -> f ProcedureTable) -> TypeState -> f TypeState
procedureTableT f (TypeState st pt ct) = (\pt' -> TypeState st pt' ct) <$> f pt

classTableT :: Functor f => (ClassTable -> f ClassTable) -> TypeState -> f TypeState
classTableT f (TypeState st pt ct) = (\ct' -> TypeState st pt ct') <$> f ct

{--}

{- Type class for syntactical elements that can be compiled -}

-- ContextGeneratable is meant for syntactical elements that can be compiled, but need additional context information to the GenState alone
class ContextGeneratable c a where
  contextGenerator :: c -> a -> Generator [MachineInstruction.Instruction]

-- Generatable is for syntactical elements that can be compiled without additional context (apart from the GenState)
class Generatable a where
  -- A generator creates a monadic action from a syntactical element that can generate code for it
  generator :: a -> Generator [MachineInstruction.Instruction]

  -- This runs a generator with some supplied state (can also be useful for testing)
  customGenerate :: a -> GenState -> Either String [MachineInstruction.Instruction]
  customGenerate e s = evalState (runExceptT $ generator e) s

  -- This runs a generator with some default empty state (mostly useful for whole programs)
  generate :: a -> Either String [MachineInstruction.Instruction]
  generate e = customGenerate e $ GenState 0 [] [] []

-- A small helper to output the generator state in case of failure
throwDiagnosticError :: String -> Generator a
throwDiagnosticError s = do
  pl <- use prefixLength
  st <- use symbolTable
  pt <- use procedureTable
  ct <- use classTable
  throwE $
    s ++ "\nDiagnostic code generation data: \n"
      ++ "Current prefix length: "
      ++ show pl
      ++ "\nCurrent symbol table:\n"
      ++ show st
      ++ "\nCurrent procedure table:\n"
      ++ show pt
      ++ "\nCurrent class table:\n"
      ++ show ct

{--}

{- Type class for representing syntactical structures whose type can be calculated -}
class Typeable a where
  -- A typifier can typify some object in the context of a TypeState, possibly resulting in a String error
  typifier :: a -> Typifier OptionalType

  -- Run a typifier, providing a TypeState
  runTypifier :: a -> TypeState -> Either String OptionalType
  runTypifier e s = runReader (runExceptT $ typifier e) s

  -- An adapter to conveniently run typifiers in the context of generators (look below for the Generatable class)
  typify :: a -> Generator OptionalType
  typify e = do
    st <- use symbolTable
    pt <- use procedureTable
    ct <- use classTable
    case runTypifier e (TypeState st pt ct) of
      Left err -> throwDiagnosticError err
      Right mt -> return mt

{--}

{- Other helper functions -}
replaceClassInTable :: ClassID -> ClassEntry -> ClassTable -> ClassTable
replaceClassInTable cid c ct = (cid, c) : filter ((/=) cid . fst) ct

lookupClassByName :: String -> ClassTable -> Maybe (ClassID, ClassEntry)
lookupClassByName name ct =
  let hits = [hit | hit@(_, ClassEntry cn _ _ _) <- ct, name == cn]
   in if hits /= [] then Just $ head hits else Nothing

isTypeLowerBoundOf :: ClassTable -> [Type] -> [[Type]] -> Bool
isTypeLowerBoundOf ct t = all (areSubtypesOf ct t)

areSubtypesOf :: ClassTable -> [Type] -> [Type] -> Bool
areSubtypesOf ct tls trs = length tls == length trs && all (== True) (zipWith (isSubtypeOf ct) tls trs)

isSubtypeOf :: ClassTable -> Type -> Type -> Bool
isSubtypeOf _ INT INT = True
isSubtypeOf _ INT (OBJ _) = False
isSubtypeOf _ (OBJ _) INT = False
isSubtypeOf ct (OBJ s) (OBJ t) = isSubclassOf ct s t

isSubclassOf :: ClassTable -> ClassName -> ClassName -> Bool
isSubclassOf ct s t =
  s == t || case lookupClassByName s ct of
    Nothing -> False
    Just (_, ClassEntry _ Nothing _ _) -> False
    Just (_, ClassEntry _ (Just tid) _ _) -> case lookup tid ct of
      Nothing -> False
      Just (ClassEntry s' _ _ _) -> isSubclassOf ct s' t

symbolDeclToType :: SymbolDeclaration -> Type
symbolDeclToType (IntDeclaration _) = INT
symbolDeclToType (ObjectDeclaration (Object t _)) = OBJ t

addParamsToSymbols :: SymbolTable -> FormalParameterList -> SymbolTable
addParamsToSymbols st [] = st
addParamsToSymbols st (p : ps) = addParamsToSymbols (addParamToSymbols st p) ps
  where
    addParamToSymbols :: SymbolTable -> SymbolDeclaration -> SymbolTable
    addParamToSymbols st' (IntDeclaration (Int n)) = addSymbol st' n INT
    addParamToSymbols st' (ObjectDeclaration (Object t n)) = addSymbol st' n (OBJ t)

addSymbol :: SymbolTable -> String -> Type -> SymbolTable
addSymbol st name t = SymbolEntry name t (length st) : st

lookupSymbol :: SymbolTable -> String -> Maybe SymbolEntry
lookupSymbol st n =
  let hits = [s | s@(SymbolEntry n' _ _) <- st, n == n']
   in if hits /= [] then Just $ head hits else Nothing

lookupFieldByName :: FieldTable -> String -> Maybe FieldEntry
lookupFieldByName [] _ = Nothing
lookupFieldByName (fe@(FieldEntry n _ _) : ft) fn
  | fn == n = Just fe
  | otherwise = lookupFieldByName ft fn

lookupClosestMatchingProc :: ClassTable -> ProcedureTable -> String -> [Type] -> Either String ProcedureEntry
lookupClosestMatchingProc ct ps n ts = do
  -- First, calculate the procedures whose name and types are compatible
  let matchingProcs = filter (matchesNameAndType ct n ts) ps
  -- Second, calculate the minimum according to the partial order of type lists
  if null matchingProcs
    then Left $ "no matching procedure definition found for procedure " ++ n ++ " with actual parameter types " ++ show ts
    else -- if there is a matching procedure, get the lower bound according to the type partial order on lists
    case getTypeLowerBoundProc ct matchingProcs matchingProcs of
      Nothing -> Left $ "ambiguous type match for procedure " ++ n ++ " with actual parameter types " ++ show ts
      Just pe -> Right pe

-- The first string is the object name, the second is the method name
lookupClosestMatchingMethod :: ClassTable -> SymbolTable -> String -> String -> [Type] -> Either String (MethodID, ProcedureEntry)
lookupClosestMatchingMethod ct st o m ts = do
  -- First, calculate the methods whose name and types are compatible
  case lookupSymbol st o of
    Nothing -> Left $ "undefined symbol " ++ o
    Just (SymbolEntry _ INT _) -> Left $ "method call on INT symbol " ++ o
    Just (SymbolEntry _ (OBJ cn) _) -> case lookupClassByName cn ct of
      Nothing -> Left $ "BUG encountered: undefined class for symbol " ++ o
      Just (_, ClassEntry cn' _ _ mt) -> do
        let matchingMethods = filter (matchesNameAndType' ct m ts) mt
        if null matchingMethods
          then Left $ "no matching method definition found for method " ++ m ++ " of class " ++ cn' ++ " with actual parameter types " ++ show ts
          else -- if there are matching methods, get the lower bound according to the type partial order on lists
          case getTypeLowerBoundMethod ct matchingMethods matchingMethods of
            Nothing -> Left $ "ambiguous type match for method " ++ m ++ " of class " ++ cn' ++ " with actual parameter types " ++ show ts
            Just me -> Right me
  where
    matchesNameAndType' ct' n ts' (_, ProcedureEntry (Signature n' ts'' _) _) = n == n' && areSubtypesOf ct' ts' ts''
    getTypeLowerBoundMethod _ [] _ = Nothing
    getTypeLowerBoundMethod ct' (lp : lps) ps =
      if isTypeLowerBoundOf ct' (getInputTypes . snd $ lp) (map (getInputTypes . snd) ps)
        then Just lp
        else getTypeLowerBoundMethod ct' lps ps

matchesNameAndType :: ClassTable -> String -> [Type] -> ProcedureEntry -> Bool
matchesNameAndType ct n ts (ProcedureEntry (Signature n' ts' _) _) = n == n' && areSubtypesOf ct ts ts'

getTypeLowerBoundProc :: ClassTable -> [ProcedureEntry] -> [ProcedureEntry] -> Maybe ProcedureEntry
getTypeLowerBoundProc _ [] _ = Nothing
getTypeLowerBoundProc ct (lp : lps) ps =
  if isTypeLowerBoundOf ct (getInputTypes lp) (map getInputTypes ps)
    then Just lp
    else getTypeLowerBoundProc ct lps ps

getInputTypes :: ProcedureEntry -> [Type]
getInputTypes (ProcedureEntry (Signature _ ts _) _) = ts

getSymbolDeclName :: SymbolDeclaration -> String
getSymbolDeclName (IntDeclaration (Int n)) = n
getSymbolDeclName (ObjectDeclaration (Object _ n)) = n

hasNameCollisions :: FormalParameterList -> Bool
hasNameCollisions [] = False
hasNameCollisions (p : fpl) = any (hasSameName p) fpl || hasNameCollisions fpl
  where
    hasSameName :: SymbolDeclaration -> SymbolDeclaration -> Bool
    hasSameName (IntDeclaration (Int n)) (IntDeclaration (Int n')) = n == n'
    hasSameName (ObjectDeclaration (Object _ n)) (ObjectDeclaration (Object _ n')) = n == n'
    hasSameName (IntDeclaration (Int n)) (ObjectDeclaration (Object _ n')) = n == n'
    hasSameName p' p'' = hasSameName p' p''

isIntType :: OptionalType -> Bool
isIntType Nothing = False
isIntType (Just INT) = True
isIntType (Just (OBJ _)) = False

-- This function calculates the required memory a procedure needs to allocate for local variables declared in its code
calculateInstructionStackMemoryRequirements :: SyntaxTree.Instruction -> Int
calculateInstructionStackMemoryRequirements (SymbolDeclarationInstruction _) = 1
calculateInstructionStackMemoryRequirements (Block (c :| [])) = calculateInstructionStackMemoryRequirements c
{- If a block has at least 2 instructions, the memory required is determined by the question if the first instruction is a block, too.
 - This is because of how blocks are compiled:
 - After compiling a block, its symbols get flushed from the table again because they should not be visible from outside
 - If the block is entered twice, the values are reset.
 - So if the (inner) block needs more space than the following instructions, its memory requirement is dominant
 - Otherwise the requirement of the following instructions is dominant - so we calculate the max value of both
 -}
calculateInstructionStackMemoryRequirements (Block (c :| (c' : cs))) = case c of
  Block _ -> max (calculateInstructionStackMemoryRequirements c) (calculateInstructionStackMemoryRequirements $ Block $ c' :| cs)
  _ -> calculateInstructionStackMemoryRequirements c + calculateInstructionStackMemoryRequirements (Block $ c' :| cs)
calculateInstructionStackMemoryRequirements (IfThen _ c) = calculateInstructionStackMemoryRequirements c
calculateInstructionStackMemoryRequirements (While _ c) = calculateInstructionStackMemoryRequirements c
calculateInstructionStackMemoryRequirements _ = 0

{--}

{- Helper generators -}
generateMethodTableInstructions :: Generator [MachineInstruction.Instruction]
generateMethodTableInstructions = do
    ct <- use classTable
    return $ map generateMethodTableInstruction ct
    where
      generateMethodTableInstruction (cid, ClassEntry _ _ _ mt) = CreateMethodTable cid (map getMethodAddress mt)
      getMethodAddress (cid, ProcedureEntry _ a) = (cid, a)
{--}

{- Class instances -}
{- Convention: Every generator ensures to clean up its state after itself -}
instance Generatable Program where
  {- Instruction layout for program:
      CreateMethodTable 0 ...
      ...
      CreateMethodTable n ...
      <code for classes>
      <code for procedures>
      # stack memory allocation for main program
      # one PushInt instruction for any declared variable
      PushInt 0
      ...
      PushInt 0
      <main program instruction code>
      Halt
  -}
  generator (Program classes procedures main) = do
    {-
      # side effects:
      # - populate class table
      # - add initializers to procedure table
      # - increase prefix length
    -}
    classInstructions <- traverse generator classes
    {-
      # side effects:
      # - populate procedure table
      # - increase prefix length
    -}
    procedureInstructions <- traverse (contextGenerator NORMAL) procedures
    let requiredStackMemory = calculateInstructionStackMemoryRequirements main
    let stackMemoryAllocationInstructions = replicate requiredStackMemory (PushInt 0)
    prefixLength += requiredStackMemory
    methodTableInstructions <- generateMethodTableInstructions
    prefixLength += length methodTableInstructions
    mainProgramInstructions <- contextGenerator MAIN main
    return $ concat classInstructions
        ++ concat procedureInstructions
        ++ stackMemoryAllocationInstructions
        ++ methodTableInstructions
        ++ mainProgramInstructions
        ++ [Halt]

instance Generatable ClassDeclaration where
  generator (Class n ps mc fields ini methods) = do
    {- Generate template class table entry with inheritance information and add fields -}
    ct <- use classTable
    let newClassID = length ct
    -- With the template, we implement inheritance
    template <-
      ( case mc of
          -- IF there is not an upper class, create empty class template
          Nothing -> return $ ClassEntry n Nothing [] []
          -- IF there IS an upper class, we essentially copy its fields and methods into our template
          Just u -> case lookupClassByName u ct of
            Nothing -> throwE $ "invalid upper class " ++ u ++ " for class " ++ n
            (Just (uid, ClassEntry _ _ uft umt)) -> return $ ClassEntry n (Just uid) uft umt
        )
    -- Now we add the template into the class table and also add the fields
    classTable .= (newClassID, template) : ct
    traverse_ (addFieldToClassEntry newClassID) fields
    {- Generate initializer as procedure with object 'this' as implicit return parameter and preceding memory allocation -}
    let initProcedure = Procedure (ProcedureHeader ("INIT_" ++ n) ps (Just $ ObjectDeclaration $ Object n "this") []) ini
    initInstructions <- contextGenerator INIT initProcedure
    {- Generate methods -}
    methodInstructions <- traverse (contextGenerator newClassID) methods
    return $ initInstructions ++ concat methodInstructions
    where
      addFieldToClassEntry :: ClassID -> SymbolDeclaration -> Generator ()
      addFieldToClassEntry cid sd = do
        ct <- use classTable
        case lookup cid ct of
          Nothing -> throwDiagnosticError "BUG encountered: trying to add field to non-existing class!"
          Just (ClassEntry cn ucc ft mt) -> case sd of
            IntDeclaration (Int sn) -> classTable %= replaceClassInTable cid (ClassEntry cn ucc (FieldEntry sn INT (length ft) : ft) mt)
            ObjectDeclaration (Object t sn) -> case lookupClassByName t ct of
              Nothing -> throwE $ "field " ++ sn ++ " of class " ++ cn ++ " has invalid type " ++ t ++ "!"
              Just _ -> classTable %= replaceClassInTable cid (ClassEntry cn ucc (FieldEntry sn (OBJ t) (length ft) : ft) mt)

instance ContextGeneratable ClassID MethodDeclaration where
  contextGenerator cid (Method (ProcedureHeader n pl mrp ps) c) = do
    {- Insert new entry / Override existing into method table of corresponding class -}
    -- Check for duplicate parameter names
    when (hasNameCollisions pl) $ throwE $ "parameter list of procedure " ++ n ++ " has duplicates"
    -- There will be one jump instruction before the actual method code starts
    prefixLength += 1
    methodCodeStart <- use prefixLength
    let newMethodEntry = ProcedureEntry (Signature n (map symbolDeclToType pl) (symbolDeclToType <$> mrp)) methodCodeStart
    updateClassTableWithNewMethod cid newMethodEntry
    {- Generate sub-procedures -}
    -- Save the old procedure table for the reset later
    oldpt <- use procedureTable
    -- Generate the sub-procedure code
    subProcedureInstructions <- traverse (contextGenerator NORMAL) ps
    {- Insert object, parameters and return parameter into symbol table -}
    ct <- use classTable
    thisParam <- case lookup cid ct of
      Nothing -> throwDiagnosticError "BUG encountered: method has no corresponding class!"
      Just (ClassEntry cn _ _ _) -> return $ ObjectDeclaration $ Object cn "this"
    let params =
          thisParam : case mrp of
            Nothing -> pl
            Just rp -> if rp `elem` pl || rp == thisParam then pl else pl ++ [rp]
    st <- use symbolTable
    symbolTable .= addParamsToSymbols st params
    stWithParams <- use symbolTable
    {- Generate method instructions, including stack memory allocation -}
    stackMemoryAllocationInstructions <- do
      let localVariableMemoryRequirements = calculateInstructionStackMemoryRequirements c
      -- if return parameter is not in parameter list, allocate a stack cell for it, too
      let returnParameterMemoryRequirements =
            case mrp of
              Nothing -> 0
              Just rp -> if rp `elem` pl then 0 else 1
      let stackMemoryRequirements = localVariableMemoryRequirements + returnParameterMemoryRequirements
      prefixLength += stackMemoryRequirements
      return $ replicate stackMemoryRequirements (PushInt 0)
    -- Generate memory init instructions for return parameter, if present, of type OBJ _ and not in param list or "this"
    returnParameterInitInstructions <- do
      case mrp of
        Nothing -> return []
        Just (IntDeclaration _) -> return [] -- Int return parameter doesn't need to be initialized b.c. it is already 0
        Just rp@(ObjectDeclaration (Object _ p)) -> do
          ct' <- use symbolTable
          case lookupSymbol ct' p of
            Nothing -> throwDiagnosticError "BUG encountered: return parameter missing from symbols!"
            Just (SymbolEntry _ _ pos) ->
              if rp `elem` pl || rp == thisParam
                then return []
                else do
                  -- generate init instructions for return symbol
                  prefixLength += 2
                  return [PushInt (-1), StoreStack pos]
    -- Generate instructions for the method itself
    methodInstructions <- contextGenerator METHOD c
    {- Create necessary instructions for return -}
    returnInstructions <- case mrp of
      Nothing -> return [Return False]
      Just rp -> case lookupSymbol stWithParams (getSymbolDeclName rp) of
        Nothing -> throwDiagnosticError "BUG encountered: return parameter missing from symbols!"
        Just (SymbolEntry _ _ p) -> return [LoadStack p, Return True]
    -- Update prefix
    prefixLength += length returnInstructions
    {- Cleanup state -}
    -- Reset symbol table
    symbolTable .= []
    -- Cleanup subprocedures from procedure table
    procedureTable .= oldpt
    {- Return generated procedure code -}
    newPrefix <- use prefixLength
    return $ [Jump newPrefix] ++ concat subProcedureInstructions ++ stackMemoryAllocationInstructions ++ returnParameterInitInstructions ++ methodInstructions ++ returnInstructions
    where
      updateClassTableWithNewMethod :: ClassID -> ProcedureEntry -> Generator ()
      updateClassTableWithNewMethod cid' pe@(ProcedureEntry s _) = do
        ct <- use classTable
        case lookup cid' ct of
          Nothing -> throwDiagnosticError "BUG encountered: method has no corresponding class!"
          Just (ClassEntry cn ucid ft mt) -> case lookupOverridableMethod ct s mt of
            -- If an overridable method doesn't exist yet, this method is new and can just be added with a new ID
            Nothing -> classTable .= replaceClassInTable cid' (ClassEntry cn ucid ft ((length mt, pe) : mt)) ct
            -- Otherwise, override the method from upper class with the same ID!
            Just (mid, ProcedureEntry _ _) -> classTable .= replaceClassInTable cid' (ClassEntry cn ucid ft (replaceMethodInTable mid pe mt)) ct

      replaceMethodInTable :: MethodID -> ProcedureEntry -> MethodTable -> MethodTable
      replaceMethodInTable mid pe mt = (mid, pe) : filter ((/=) mid . fst) mt

      lookupOverridableMethod :: ClassTable -> Signature -> MethodTable -> Maybe (MethodID, ProcedureEntry)
      lookupOverridableMethod _ _ [] = Nothing
      lookupOverridableMethod ct s ((mid, p@(ProcedureEntry s' _)) : mt) =
        if overridesSig ct s s'
          then Just (mid, p)
          else lookupOverridableMethod ct s mt
        where
          overridesSig ct' (Signature name ts rt) (Signature name' ts' rt') = name == name' && ts == ts' && overridesRet ct' rt rt'
          overridesRet _ Nothing Nothing = True
          overridesRet _ Nothing (Just _) = False
          overridesRet _ (Just _) Nothing = False
          overridesRet ct' (Just t) (Just t') = isSubtypeOf ct' t t'

instance ContextGeneratable ProcedureKind ProcedureDeclaration where
  contextGenerator kind (Procedure (ProcedureHeader n pl mrp ps) c) = do
    {- Insert new entry into procedure table -}
    -- Check for duplicate parameter names
    when (hasNameCollisions pl) $ throwE $ "parameter list of procedure " ++ n ++ " has duplicates"
    pt <- use procedureTable
    oldPrefix <- use prefixLength
    -- Address of new procedure must be the old prefix + 1, because of the jump instruction at the beginning that must be skipped
    let newProcedureEntry = ProcedureEntry (Signature n (map symbolDeclToType pl) (symbolDeclToType <$> mrp)) (oldPrefix + 1)
    -- Update state with new procedure entry
    procedureTable .= newProcedureEntry : pt
    {- Generate sub-procedures -}
    -- Increase prefix by 1 - we know there will be 1 new jump instruction for the upper procedure before the subprocedures
    prefixLength += 1
    -- Generate code for sub procedures
    subProcedureInstructions <- traverse (contextGenerator NORMAL) ps
    {- Insert parameters and return parameter into symbol table -}
    let params = case mrp of
          Nothing -> pl
          Just rp -> if rp `elem` pl then pl else pl ++ [rp]
    st <- use symbolTable
    symbolTable .= addParamsToSymbols st params
    stWithParams <- use symbolTable
    {- Generate procedure code, including stack memory allocation and, in case of initializers, heap memory allocation-}
    stackMemoryAllocationInstructions <- do
      let localVariableMemoryRequirements = calculateInstructionStackMemoryRequirements c
      -- if return parameter is not in parameter list, allocate a stack cell for it, too
      let returnParameterMemoryRequirements =
            case mrp of
              Nothing -> 0
              Just rp -> if rp `elem` pl then 0 else 1
      let stackMemoryRequirements = localVariableMemoryRequirements + returnParameterMemoryRequirements
      prefixLength += stackMemoryRequirements
      return $ replicate stackMemoryRequirements (PushInt 0)
    -- Generate memory init instructions for return parameter, if present, of type OBJ _ and not in param list
    returnParameterInitInstructions <- do
      case mrp of
        Nothing -> return []
        Just (IntDeclaration _) -> return [] -- Int return parameter doesn't need to be initialized b.c. it is already 0
        Just rp@(ObjectDeclaration (Object _ p)) -> do
          ct' <- use symbolTable
          case lookupSymbol ct' p of
            Nothing -> throwDiagnosticError "BUG encountered: return parameter missing from symbols!"
            Just (SymbolEntry _ _ pos) ->
              if rp `elem` pl
                then return []
                else do
                  -- generate init instructions for return symbol
                  prefixLength += 2
                  return [PushInt (-1), StoreStack pos]
    -- IF INIT procedure: generate memory allocation instructions and update prefix accordingly
    heapMemoryAllocationInstructions <- case kind of
      INIT -> do
        prefixLength += 2
        case mrp of
          Nothing -> throwDiagnosticError "BUG encountered: initializer without return value!"
          Just rp -> case lookupSymbol stWithParams (getSymbolDeclName rp) of
            Nothing -> throwDiagnosticError "BUG encountered: return parameter missing from symbols!"
            Just (SymbolEntry _ t p) -> case t of
              INT -> throwDiagnosticError "BUG encountered: initializer with INT return value!"
              OBJ s -> do
                ct <- use classTable
                case lookupClassByName s ct of
                  Nothing -> throwDiagnosticError "BUG encountered: initializer with invalid return type!"
                  -- generate heap allocation instructions, along with instructions to correctly initialize all fields
                  Just (cid, ClassEntry _ _ ft _) -> do
                    let fieldInitCmds = concatMap inits ft
                    prefixLength += length fieldInitCmds
                    return $ [AllocateHeap (length ft) cid, StoreStack p] ++ fieldInitCmds
                    where
                      inits (FieldEntry _ (OBJ _) heapFrameIndex) = [LoadStack p, PushInt (-1), StoreHeap heapFrameIndex]
                      inits (FieldEntry _ INT heapFrameIndex) = [LoadStack p, PushInt 0, StoreHeap heapFrameIndex]
      _ -> return []
    -- generate the instructions
    procedureInstructions <- contextGenerator PROCEDURE c
    {- Create necesary instructions for return -}
    returnInstructions <- case mrp of
      Nothing -> return [Return False]
      Just rp -> case lookupSymbol stWithParams (getSymbolDeclName rp) of
        Nothing -> throwDiagnosticError "BUG encountered: return parameter missing from symbols!"
        Just (SymbolEntry _ _ p) -> return [LoadStack p, Return True]
    -- Update prefix
    prefixLength += length returnInstructions
    {- Cleanup state -}
    -- Reset symbol table
    symbolTable .= []
    -- Reset procedure table to only contain the topmost procedure, not the subprocedures
    procedureTable .= newProcedureEntry : pt
    {- Return generated procedure code -}
    newPrefix <- use prefixLength
    return $ [Jump newPrefix] ++ concat subProcedureInstructions ++ stackMemoryAllocationInstructions ++ returnParameterInitInstructions ++ heapMemoryAllocationInstructions ++ procedureInstructions ++ returnInstructions

instance Generatable Call where
  generator (SymbolReference (NameReference n)) = do
    -- lookup name
    st <- use symbolTable
    pos <- case lookupSymbol st n of
      Nothing -> throwE $ "undefined symbol " ++ n
      Just (SymbolEntry _ _ p) -> return p
    -- the resulting instruction is just a load on the symbol's position
    prefixLength += 1
    return [LoadStack pos]
  generator (SymbolReference (FieldReference o f)) = do
    -- lookup object
    st <- use symbolTable
    (objType, objPos) <- case lookupSymbol st o of
      Nothing -> throwE $ "undefined symbol " ++ o
      Just (SymbolEntry _ INT _) -> throwE "trying to access field of a non-object"
      Just (SymbolEntry _ (OBJ t) p) -> return (t, p)
    ct <- use classTable
    -- lookup field
    fieldPos <- case lookupClassByName objType ct of
      Nothing -> throwDiagnosticError $ "BUG encountered: invalid class for object " ++ o
      Just (_, ClassEntry _ _ ft _) -> case lookupFieldByName ft f of
        Nothing -> throwE $ "invalid field " ++ f ++ " reference for object " ++ o
        Just (FieldEntry _ _ p) -> return p
    -- the resulting instructions are a stack load of the symbols position to push the object's address, then a heap load of the referenced field
    prefixLength += 2
    return [LoadStack objPos, LoadHeap fieldPos]
  generator (Call (NameReference n) apl) = do
    -- lookup procedure
    paramTypes <- traverse typify apl
    ct <- use classTable
    pt <- use procedureTable
    procAddress <- case sequenceA paramTypes of
      Nothing -> throwE "type error: empty type in parameter hole!"
      Just ts -> case lookupClosestMatchingProc ct pt n ts of
        Left e -> throwE e
        Right (ProcedureEntry _ a) -> return a
    -- generate parameter loading instructions and procedure call
    paramInstructions <- traverse generator apl
    prefixLength += 1 -- we only generate 1 additional instruction - the procedure call
    -- the result is all the instructions to generate the parameter expressions in order followed by the procedure call
    return $ concat paramInstructions ++ [CallProcedure procAddress (length apl)]
  generator (Call (FieldReference o m) apl) = do
    -- lookup method from method table
    paramTypes <- traverse typify apl
    ct <- use classTable
    st <- use symbolTable
    methodID <- case sequenceA paramTypes of
      Nothing -> throwE "type error: empty type in parameter hole!"
      Just ts -> case lookupClosestMatchingMethod ct st o m ts of
        Left e -> throwE e
        Right (mid, ProcedureEntry _ _) -> return mid
    -- generate parameter loading instructions and method call
    -- first, lookup the object position
    objPos <- case lookupSymbol st o of
      Nothing -> throwE $ "call on invalid object symbol " ++ o
      Just (SymbolEntry _ _ p) -> return p
    let objectLoadingInstruction = [LoadStack objPos]
    prefixLength += 1
    -- then generate the instructions for all the actual parameters
    paramInstructions <- traverse generator apl
    prefixLength += 1
    return $ objectLoadingInstruction ++ concat paramInstructions ++ [CallMethod methodID (length apl)]

instance Typeable Call where
  typifier (SymbolReference (NameReference n)) = do
    st <- view symbolTableT
    case lookupSymbol st n of
      Nothing -> throwE $ "undefined variable in expression: " ++ n
      -- an symbol's type is just the type declared in the symbol table
      Just (SymbolEntry _ t _) -> return $ Just t
  typifier (SymbolReference (FieldReference o f)) = do
    -- lookup object
    st <- view symbolTableT
    (objType, _) <- case lookupSymbol st o of
      Nothing -> throwE $ "undefined symbol " ++ o
      Just (SymbolEntry _ INT _) -> throwE "trying to access field of a non-object"
      Just (SymbolEntry _ (OBJ t) p) -> return (t, p)
    ct <- view classTableT
    -- lookup field
    case lookupClassByName objType ct of
      Nothing -> throwE $ "BUG encountered: invalid class for object " ++ o
      Just (_, ClassEntry _ _ ft _) -> case lookupFieldByName ft f of
        Nothing -> throwE $ "invalid field " ++ f ++ " reference for object " ++ o
        -- a field reference's type is determined by the field type in the class declaration
        Just (FieldEntry _ t _) -> return $ Just t
  typifier (Call (NameReference n) apl) = do
    paramTypes <- traverse typifier apl
    pt <- view procedureTableT
    ct <- view classTableT
    case sequenceA paramTypes of
      -- IF one of the types is Nothing, there is a hole!
      Nothing -> throwE "type error: empty type in parameter hole!"
      Just ts -> case lookupClosestMatchingProc ct pt n ts of
        Left e -> throwE e
        -- the type of the procedure call is just the type of the closest matching procedure
        Right (ProcedureEntry (Signature _ _ rt) _) -> return rt
  typifier (Call (FieldReference o m) apl) = do
    paramTypes <- traverse typifier apl
    ct <- view classTableT
    st <- view symbolTableT
    case sequenceA paramTypes of
      Nothing -> throwE "type error: empty type in parameter hole!"
      Just ts -> case lookupClosestMatchingMethod ct st o m ts of
        Left e -> throwE e
        -- again, the lookup function does the magic here, we just return its result
        Right (_, ProcedureEntry (Signature _ _ rt) _) -> return rt

-- Every instruction can be in procedure-, method-, main-program- context or inside of an instruction block
data InstructionContext = PROCEDURE | METHOD | MAIN | INNER

instance ContextGeneratable InstructionContext SyntaxTree.Instruction where
  contextGenerator ctxt (Assignment (NameReference n) e) = do
    ct <- use classTable
    st <- use symbolTable
    (symType, symPos) <- case lookupSymbol st n of
      Nothing -> throwE $ "assignment to undefined variable " ++ n
      Just (SymbolEntry _ t p) -> return (t, p)
    mt <- typify e
    case mt of
      Nothing -> throwE $ "variable " ++ n ++ " was assigned an expression with empty type"
      Just ty ->
        if isSubtypeOf ct ty symType
          then do
            -- Type is correct - we can generate the instructions
            eInstructions <- generator e
            prefixLength += 1
            updateSymbolTableDependingOnInstructionContext ctxt st
            return $ eInstructions ++ [StoreStack symPos]
          else throwDiagnosticError $ "variable " ++ n ++ " was assigned an expression with incompatible type"
  contextGenerator _ (Assignment (FieldReference o f) e) = do
    -- lookup object
    st <- use symbolTable
    (objType, objPos) <- case lookupSymbol st o of
      Nothing -> throwE $ "undefined symbol " ++ o
      Just (SymbolEntry _ INT _) -> throwE "trying to access field of a non-object"
      Just (SymbolEntry _ (OBJ t) p) -> return (t, p)
    ct <- use classTable
    -- lookup field
    (fieldType, fieldPos) <- case lookupClassByName objType ct of
      Nothing -> throwDiagnosticError $ "BUG encountered: invalid class for object " ++ o
      Just (_, ClassEntry _ _ ft _) -> case lookupFieldByName ft f of
        Nothing -> throwE $ "invalid field " ++ f ++ " reference for object " ++ o
        Just (FieldEntry _ t p) -> return (t, p)
    -- check types
    exprType <- typify e
    case exprType of
      Nothing -> throwE "type error: trying to assign empty return value"
      Just ty ->
        if isSubtypeOf ct ty fieldType
          then return ()
          else throwE $ "type error: cannot assign to field, as type " ++ show ty ++ " of expression is not a subtype of field " ++ f ++ " with type " ++ show fieldType ++ " of object " ++ o
    -- generate instructions
    let objAddrLoadInstruction = [LoadStack objPos]
    prefixLength += 1
    expressionInstructions <- generator e
    let storeInstruction = [StoreHeap fieldPos]
    prefixLength += 1
    return $ objAddrLoadInstruction ++ expressionInstructions ++ storeInstruction
  contextGenerator ctxt (SymbolDeclarationInstruction (IntDeclaration (Int n))) = do
    -- Assemble new symbol entry and add it
    st <- use symbolTable
    symbolTable .= addSymbol st n INT
    st' <- use symbolTable
    -- lookup position of new symbol
    pos <- case lookupSymbol st' n of
      Nothing -> throwDiagnosticError "BUG encountered: impossibly, the symbol we just added vanished"
      Just (SymbolEntry _ _ p) -> return p
    -- return instructions and update prefix as well symbol table
    prefixLength += 2
    -- possible optimization: if the context isn't INNER, the instructions can be omitted altogether
    updateSymbolTableDependingOnInstructionContext ctxt st
    return [PushInt 0, StoreStack pos]
  contextGenerator ctxt (SymbolDeclarationInstruction (ObjectDeclaration (Object t n))) = do
    -- Check if class is valid
    ct <- use classTable
    case lookupClassByName t ct of
      Nothing -> throwE $ "invalid class " ++ t ++ " for object " ++ n
      Just _ -> return ()
    -- Assemble new symbol entry and add it
    st <- use symbolTable
    symbolTable .= addSymbol st n (OBJ t)
    st' <- use symbolTable
    -- look up position of new symbol
    pos <- case lookupSymbol st' n of
      Nothing -> throwDiagnosticError "BUG encountered: impossibly, the symbol we just added vanished"
      Just (SymbolEntry _ _ p) -> return p
    -- return instructions and update prefix as well as symbol table
    prefixLength += 2
    -- possible optimization: of the context isn't INNER, the instructions can be omitted altogether
    updateSymbolTableDependingOnInstructionContext ctxt st
    -- An object declaration doesn't allocate memory, the address will be invalid until the object is initialized
    return [PushInt (-1), StoreStack pos]
  contextGenerator _ (CallInstruction call) = do
    t <- typify call
    case t of
      Nothing -> generator call
      Just _ -> throwE "type error: we can only fit empty return values here"
  contextGenerator _ (SyntaxTree.Read n) = do
    st <- use symbolTable
    pos <- case lookupSymbol st n of
      Nothing -> throwE $ "undefined symbol " ++ n
      Just (SymbolEntry _ (OBJ _) _) -> throwE $ "type error: " ++ n ++ " is an object, can't read an integer into it"
      Just (SymbolEntry _ INT p) -> return p
    prefixLength += 2
    return [MachineInstruction.Read, StoreStack pos]
  contextGenerator _ (Block cs) = do
    -- save old symbol table for the reset after generating the instructions - this implements scoping
    st <- use symbolTable
    cmds <- traverse (contextGenerator INNER) cs
    symbolTable .= st
    return $ concat cmds
  contextGenerator _ (IfThen cond cmd) = do
    st <- use symbolTable
    condInstructions <- generator cond
    prefixLength += 1
    bodyInstructions <- contextGenerator INNER cmd
    symbolTable .= st
    p <- use prefixLength
    return $ condInstructions ++ [JumpIfFalse p] ++ bodyInstructions
  contextGenerator _ (While cond cmd) = do
    st <- use symbolTable
    oldPrefix <- use prefixLength
    condInstructions <- generator cond
    prefixLength += 1
    bodyInstructions <- contextGenerator INNER cmd
    prefixLength += 1
    symbolTable .= st
    newPrefix <- use prefixLength
    return $ condInstructions ++ [JumpIfFalse newPrefix] ++ bodyInstructions ++ [Jump oldPrefix]
  contextGenerator _ (SyntaxTree.PrintI e) = do
    t <- typify e
    case t of
      Nothing -> throwE "type error: we can only fit integer values here"
      Just (OBJ _) -> throwE "type error: we can only fit integer values here"
      Just INT -> do
        eCmds <- generator e
        prefixLength += 1
        return $ eCmds ++ [PrintInt]
  contextGenerator _ (PrintS msg) = do
    prefixLength += 1
    return [PrintStr msg]
  contextGenerator _ (PrintLnS msg) = do
    prefixLength += 1
    return [PrintStrLn msg]
  contextGenerator _ Error = do
    prefixLength += 1
    return [Halt]

{- This action generator is for correctly setting the symbol table after an instruction is generated.
 - The correct behavior depends on the context:
 - Method-, Procedure- and Main-Program-Context: The symbol table should be reset
 - Inner instruction as part of an instruction block: The symbol table should NOT be reset
 - Following instructions would need access to the new symbols that preceding instructions in the same block generate, for example:
 -  WHILE 1 = 1 { VAR x
 -                x := 0 }
 - A block on the other hand will always reset the symbol table, to respect scoping rules
 - For example, the following should be illegal:
 - PROCEDURE foo(VAR x) { { VAR Y } Y := 0 }
 -}
updateSymbolTableDependingOnInstructionContext :: InstructionContext -> SymbolTable -> Generator ()
updateSymbolTableDependingOnInstructionContext INNER _ = return ()
updateSymbolTableDependingOnInstructionContext _ st = symbolTable .= st

instance Generatable Condition where
  generator (Comparison e r e') = do
    -- check types
    t <- typify e
    t' <- typify e'
    case t of
      Nothing -> throwE "type error: conditions can only be evaluated on integers"
      Just ty -> when (ty /= INT || t /= t') $ throwE "type error: conditions can only be evaluated on integers"
    eInstructions <- generator e
    e'Instructions <- generator e'
    let newCmds = eInstructions ++ e'Instructions ++ [CombineBinary $ conv r]
    prefixLength += 1
    return newCmds
    where
      conv SyntaxTree.Equals = MachineInstruction.Equals
      conv SyntaxTree.Smaller = MachineInstruction.Smaller
      conv SyntaxTree.Greater = MachineInstruction.Greater
  generator (Negation c) = do
    cmds <- generator c
    let newCmds = cmds ++ [CombineUnary Not]
    prefixLength += 1
    return newCmds

-- this generator expects the expression to be well-typed which needs to be ensured before calling
instance Generatable Expression where
  generator (Expression ((s, t) :| sts)) = do
    -- If there is a plus, we just generate the factor (works also if s has type OBJ _)
    -- If there is a minus, we generate PushInt 0 before and CombineBinaryMinus after the factor, calculating its negated value
    firstFactorInstructions <- case s of
      SyntaxTree.Plus -> generator t
      SyntaxTree.Minus -> do
        prefixLength += 1
        factorInstructions <- generator t
        prefixLength += 1
        return $ [PushInt 0] ++ factorInstructions ++ [CombineBinary MachineInstruction.Minus]
    tsInstructions <- traverse stGenerator sts
    return $ firstFactorInstructions ++ concat tsInstructions
    where
      stGenerator (s', t') = do
        tInstructions <- generator t'
        prefixLength += 1
        let signInstruction = [CombineBinary $ conv s']
        return $ tInstructions ++ signInstruction
      conv SyntaxTree.Plus = MachineInstruction.Plus
      conv SyntaxTree.Minus = MachineInstruction.Minus

instance Typeable Expression where
  typifier (Expression ((_, t) :| ts)) = do
    ttype <- typifier t
    if null ts
      then return ttype
      else do
        -- we just typify all the terms - if there are multiple terms and one is not an integer, the type is invalid
        ttypes <- traverse (typifier . snd) ts
        if isIntType ttype && all isIntType ttypes
          then return $ Just INT
          else throwE "invalid type: non-integer value in algebraic term!"

instance Generatable Term where
  generator (Term f ofs) = do
    fInstructions <- generator f
    ofsInstructions <- traverse ofsGenerator ofs
    return $ fInstructions ++ concat ofsInstructions
    where
      ofsGenerator (o, f') = do
        fInstructions <- generator f'
        prefixLength += 1
        let oInstruction = [CombineBinary $ conv o]
        return $ fInstructions ++ oInstruction
      conv SyntaxTree.Times = MachineInstruction.Times
      conv SyntaxTree.Divide = MachineInstruction.Divide

instance Typeable Term where
  typifier (Term f ofs) = do
    ftype <- typifier f
    if null ofs
      then return ftype
      else do
        -- we typify all factors - again, if there are multiple factors, one of them being of type OBJ _ will make the type invalid
        ftypes <- traverse (typifier . snd) ofs
        if isIntType ftype && all isIntType ftypes
          then return $ Just INT
          else throwE "invalid type: non-integer value in algebraic term!"

instance Generatable Factor where
  generator (CallFactor c) = generator c
  generator (ClassInstantiation cn apl) = generator (Call (NameReference $ "INIT_" ++ cn) apl)
  generator (Integer n) = do
    prefixLength += 1
    return [PushInt n]
  generator (CompositeFactor e) = generator e

-- these don't introduce much new logic, just syntax tree decomposition for the most part
instance Typeable Factor where
  typifier (CallFactor c) = typifier c
  -- a class instantiation unsurprisingly has the same type as the corresponding initializer/constructor
  typifier (ClassInstantiation cn apl) = typifier (Call (NameReference $ "INIT_" ++ cn) apl)
  typifier (Integer _) = return $ Just INT
  typifier (CompositeFactor e) = typifier e

{--}