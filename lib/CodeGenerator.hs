{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda"#-}

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
    SymbolName,
    SymbolReference (FieldReference, NameReference),
    Term (..),
  )

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
calculateStackMemoryRequirement :: SyntaxTree.Instruction -> Int
calculateStackMemoryRequirement (SymbolDeclarationInstruction _) = 1
calculateStackMemoryRequirement (Block (c :| [])) = calculateStackMemoryRequirement c
{- If a block has at least 2 instructions, the memory required is determined by the question if the first instruction is a block, too.
 - This is because of how blocks are compiled:
 - After compiling a block, its symbols get flushed from the table again because they should not be visible from outside
 - If the block is entered twice, the values are reset.
 - So if the (inner) block needs more space than the following instructions, its memory requirement is dominant
 - Otherwise the requirement of the following instructions is dominant - so we calculate the max value of both
 -}
calculateStackMemoryRequirement (Block (c :| (c' : cs))) = case c of
  Block _ -> max (calculateStackMemoryRequirement c) (calculateStackMemoryRequirement $ Block $ c' :| cs)
  _ -> calculateStackMemoryRequirement c + calculateStackMemoryRequirement (Block $ c' :| cs)
calculateStackMemoryRequirement (IfThen _ c) = calculateStackMemoryRequirement c
calculateStackMemoryRequirement (While _ c) = calculateStackMemoryRequirement c
calculateStackMemoryRequirement _ = 0

{--}

{- Helper generators -}
generateMethodTableInstructions :: Generator [MachineInstruction.Instruction]
generateMethodTableInstructions = do
  ct <- use classTable
  return $ map generateMethodTableInstruction ct
  where
    generateMethodTableInstruction (cid, ClassEntry _ _ _ mt) = CreateMethodTable cid (map getMethodAddress mt)
    getMethodAddress (cid, ProcedureEntry _ a) = (cid, a)

createClassTableEntry :: ClassName -> Maybe ClassName -> [SymbolDeclaration] -> Generator ClassID
createClassTableEntry name muname fields = do
  {- Generate template class table entry with inheritance information and add fields -}
  ct <- use classTable
  let newClassID = length ct
  -- With the template, we implement inheritance
  template <-
    ( case muname of
        -- IF there is not an upper class, create empty class template
        Nothing -> return $ ClassEntry name Nothing [] []
        -- IF there IS an upper class, we essentially copy its fields and methods into our template
        Just uname -> case lookupClassByName uname ct of
          Nothing -> throwE $ "invalid upper class " ++ uname ++ " for class " ++ name
          (Just (uid, ClassEntry _ _ uft umt)) -> return $ ClassEntry name (Just uid) uft umt
      )
  -- Now we add the template into the class table and also add the fields
  classTable .= (newClassID, template) : ct
  traverse_ (addFieldToClassEntry newClassID) fields
  return newClassID
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

generateInitializer :: ClassName -> FormalParameterList -> SyntaxTree.Instruction -> Generator [MachineInstruction.Instruction]
generateInitializer name parameters code = do
  let initProcedure = Procedure (ProcedureHeader ("INIT_" ++ name) parameters (Just $ ObjectDeclaration $ Object name "this") []) code
  contextGenerator INIT initProcedure

addMethodToClassTable :: ClassID -> SymbolName -> FormalParameterList -> Maybe SymbolDeclaration -> Generator ()
addMethodToClassTable classID name parameters mReturnParameter = do
  -- Check for duplicate parameter names
  when (hasNameCollisions parameters) $ throwE $ "parameter list of procedure " ++ name ++ " has duplicates"
  a <- use prefixLength
  let newMethodEntry = ProcedureEntry (Signature name (map symbolDeclToType parameters) (symbolDeclToType <$> mReturnParameter)) a
  updateClassTableWithNewMethod classID newMethodEntry
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
        overridesSig ct' (Signature mname ts rt) (Signature mname' ts' rt') = mname == mname' && ts == ts' && overridesRet ct' rt rt'
        overridesRet _ Nothing Nothing = True
        overridesRet _ Nothing (Just _) = False
        overridesRet _ (Just _) Nothing = False
        overridesRet ct' (Just t) (Just t') = isSubtypeOf ct' t t'

addMethodParametersToSymbolTable :: ClassID -> FormalParameterList -> Maybe SymbolDeclaration -> Generator SymbolDeclaration
addMethodParametersToSymbolTable classID parameters mReturnParameter = do
  ct <- use classTable
  thisParam <- case lookup classID ct of
    Nothing -> throwDiagnosticError "BUG encountered: method has no corresponding class!"
    Just (ClassEntry cn _ _ _) -> return $ ObjectDeclaration $ Object cn "this"
  let params =
        thisParam : case mReturnParameter of
          Nothing -> parameters
          Just rp -> if rp `elem` parameters || rp == thisParam then parameters else parameters ++ [rp]
  st <- use symbolTable
  symbolTable .= addParamsToSymbols st params
  return thisParam

generateStackMemoryAllocationInstructions :: FormalParameterList -> Maybe SymbolDeclaration -> SyntaxTree.Instruction -> Generator [MachineInstruction.Instruction]
generateStackMemoryAllocationInstructions parameters mReturnParameter code = do
  prefixLength += stackMemoryRequirements
  return $ replicate stackMemoryRequirements (PushInt 0)
  where
    stackMemoryRequirements =
      calculateStackMemoryRequirement code + case mReturnParameter of
        Nothing -> 0
        -- if return parameter is not in parameter list, allocate a stack cell for it, too
        Just rp -> if rp `elem` parameters then 0 else 1

generateMethodReturnParameterInitInstructions :: SymbolDeclaration -> FormalParameterList -> Maybe SymbolDeclaration -> Generator [MachineInstruction.Instruction]
generateMethodReturnParameterInitInstructions thisParameter parameters mReturnParameter = case mReturnParameter of
  Nothing -> return []
  Just (IntDeclaration _) -> return [] -- Int return parameter doesn't need to be initialized b.c. it is already 0
  Just rp@(ObjectDeclaration (Object _ p)) -> do
    st' <- use symbolTable
    case lookupSymbol st' p of
      Nothing -> throwDiagnosticError "BUG encountered: return parameter missing from symbols!"
      Just (SymbolEntry _ _ pos) ->
        if rp `elem` parameters || rp == thisParameter
          then return []
          else do
            -- generate init instructions for return symbol
            prefixLength += 2
            return [PushInt (-1), StoreStack pos]

generateReturnInstructions :: Maybe SymbolDeclaration -> Generator [MachineInstruction.Instruction]
generateReturnInstructions mReturnParameter = do
  st <- use symbolTable
  instructions <- case mReturnParameter of
    Nothing -> return [Return False]
    Just rp -> case lookupSymbol st (getSymbolDeclName rp) of
      Nothing -> throwDiagnosticError "BUG encountered: return parameter missing from symbols!"
      Just (SymbolEntry _ t p) -> do
        -- check if return parameter was shadowed by variable of incompatible type!
        checkTypeCompatibility (Just t) (symbolDeclToType rp)
        return [LoadStack p, Return True]
  prefixLength += length instructions
  return instructions

addToProcedureTable :: SymbolName -> FormalParameterList -> Maybe SymbolDeclaration -> Generator ()
addToProcedureTable name parameters mReturnParameter = do
  -- Check for duplicate parameter names
  when (hasNameCollisions parameters) $ throwE $ "parameter list of procedure " ++ name ++ " has duplicates"
  pt <- use procedureTable
  prefix <- use prefixLength
  -- Address of new procedure must be the old prefix + 1, because of the jump instruction at the beginning that must be skipped
  let newProcedureEntry = ProcedureEntry (Signature name (map symbolDeclToType parameters) (symbolDeclToType <$> mReturnParameter)) prefix
  -- Update state with new procedure entry
  procedureTable .= newProcedureEntry : pt

addProcedureParametersToSymbolTable :: FormalParameterList -> Maybe SymbolDeclaration -> Generator ()
addProcedureParametersToSymbolTable parameters mReturnParameter = do
  let params = case mReturnParameter of
        Nothing -> parameters
        Just rp -> if rp `elem` parameters then parameters else parameters ++ [rp]
  st <- use symbolTable
  symbolTable .= addParamsToSymbols st params

generateProcedureReturnParameterInitInstructions :: FormalParameterList -> Maybe SymbolDeclaration -> Generator [MachineInstruction.Instruction]
generateProcedureReturnParameterInitInstructions parameters mReturnParameter = do
  case mReturnParameter of
    Nothing -> return []
    Just (IntDeclaration _) -> return [] -- Int return parameter doesn't need to be initialized b.c. it is already 0
    Just rp@(ObjectDeclaration (Object _ p)) -> do
      ct' <- use symbolTable
      case lookupSymbol ct' p of
        Nothing -> throwDiagnosticError "BUG encountered: return parameter missing from symbols!"
        Just (SymbolEntry _ _ pos) ->
          if rp `elem` parameters
            then return []
            else do
              -- generate init instructions for return symbol
              prefixLength += 2
              return [PushInt (-1), StoreStack pos]

generateHeapMemoryAllocationInstructions :: ProcedureKind -> Maybe SymbolDeclaration -> Generator [MachineInstruction.Instruction]
generateHeapMemoryAllocationInstructions INIT mReturnParameter = do
  stWithParams <- use symbolTable
  prefixLength += 2
  case mReturnParameter of
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
generateHeapMemoryAllocationInstructions _ _ = return []

lookupSymbolTypeByNameT :: SymbolName -> Typifier OptionalType
lookupSymbolTypeByNameT name = do
  st <- view symbolTableT
  case lookupSymbol st name of
    Nothing -> throwE $ "undefined variable in expression: " ++ name
    -- an symbol's type is just the type declared in the symbol table
    Just (SymbolEntry _ t _) -> return $ Just t

lookupFieldTypeByTypeAndFieldName :: OptionalType -> SymbolName -> Typifier OptionalType
lookupFieldTypeByTypeAndFieldName mt field = do
  ct <- view classTableT
  case mt of
    Nothing -> throwE ""
    Just INT -> throwE ""
    Just (OBJ cname) -> case lookupClassByName cname ct of
      Nothing -> throwE $ "BUG encountered: invalid class " ++ cname ++ "!"
      Just (_, ClassEntry _ _ ft _) -> case lookupFieldByName ft field of
        Nothing -> throwE $ "invalid field " ++ field ++ " reference for class " ++ cname ++ "!"
        -- a field reference's type is determined by the field type in the class declaration
        Just (FieldEntry _ t _) -> return $ Just t

lookupSymbolPosByName :: SymbolName -> Generator Position
lookupSymbolPosByName name = fst <$> lookupSymbolPosAndTypeByName name

lookupSymbolTypeByName :: SymbolName -> Generator Type
lookupSymbolTypeByName name = snd <$> lookupSymbolPosAndTypeByName name

lookupSymbolPosAndTypeByName :: SymbolName -> Generator (Position, Type)
lookupSymbolPosAndTypeByName name = do
  st <- use symbolTable
  case lookupSymbol st name of
    Nothing -> throwE $ "undefined symbol " ++ name
    Just (SymbolEntry _ t pos) -> return (pos, t)

lookupFieldPosByTypeAndFieldName :: Type -> SymbolName -> Generator Position
lookupFieldPosByTypeAndFieldName t field = do
  (pos, _) <- lookupFieldPosAndTypeByTypeAndFieldName t field
  return pos

lookupFieldPosAndTypeByTypeAndFieldName :: Type -> SymbolName -> Generator (Position, Type)
lookupFieldPosAndTypeByTypeAndFieldName t field = do
  ct <- use classTable
  case t of
    INT -> throwE "trying to access field of a non-object!"
    OBJ cname -> case lookupClassByName cname ct of
      Nothing -> throwDiagnosticError $ "BUG encountered: invalid class " ++ cname ++ "!"
      Just (_, ClassEntry _ _ ft _) -> case lookupFieldByName ft field of
        Nothing -> throwE $ "invalid field " ++ field ++ " for class " ++ cname ++ "!"
        Just (FieldEntry _ t' p) -> return (p, t')

calculateMatchingSetMinimumAddressForProcedureInvocation :: SymbolName -> [OptionalType] -> Generator CodeAddress
calculateMatchingSetMinimumAddressForProcedureInvocation name types = do
  ct <- use classTable
  pt <- use procedureTable
  case sequenceA types of
    Nothing -> throwE "type error: empty type in parameter hole"
    Just ts -> case lookupClosestMatchingProc ct pt name ts of
      Left e -> throwE e
      Right (ProcedureEntry _ a) -> return a

calculateMatchingSetMinimumTypeForProcedureInvocationT :: SymbolName -> [OptionalType] -> Typifier ReturnType
calculateMatchingSetMinimumTypeForProcedureInvocationT name types = do
  ct <- view classTableT
  pt <- view procedureTableT
  case sequenceA types of
    Nothing -> throwE "type error: empty type in parameter hole"
    Just ts -> case lookupClosestMatchingProc ct pt name ts of
      Left e -> throwE e
      Right (ProcedureEntry (Signature _ _ rt) _) -> return rt

calculateMatchingSetMinimumIDForMethodInvocation :: SymbolName -> SymbolName -> [OptionalType] -> Generator MethodID
calculateMatchingSetMinimumIDForMethodInvocation objName methodName types = do
  ct <- use classTable
  st <- use symbolTable
  case sequenceA types of
    Nothing -> throwE "type error: empty type in parameter hole!"
    Just ts -> case lookupClosestMatchingMethod ct st objName methodName ts of
      Left e -> throwE e
      Right (mid, ProcedureEntry _ _) -> return mid

calculateMatchingSetMinimumTypeForMethodInvocationT :: SymbolName -> SymbolName -> [OptionalType] -> Typifier ReturnType
calculateMatchingSetMinimumTypeForMethodInvocationT objName methodName types = do
  ct <- view classTableT
  st <- view symbolTableT
  case sequenceA types of
    Nothing -> throwE "type error: empty type in parameter hole!"
    Just ts -> case lookupClosestMatchingMethod ct st objName methodName ts of
      Left e -> throwE e
      Right (_, ProcedureEntry (Signature _ _ rt) _) -> return rt

checkTypeCompatibility :: OptionalType -> Type -> Generator ()
checkTypeCompatibility Nothing _ = throwE "empty type has no supertype"
checkTypeCompatibility (Just s) t = do
  ct <- use classTable
  if isSubtypeOf ct s t
    then return ()
    else throwE $ "incompatible types " ++ show s ++ " and " ++ show t ++ "!"

addSymbolToTable :: SymbolName -> Type -> Generator Position
addSymbolToTable name t = do
  -- Assemble new symbol entry and add it
  st <- use symbolTable
  symbolTable .= addSymbol st name t
  st' <- use symbolTable
  -- lookup position of new symbol
  case lookupSymbol st' name of
    Nothing -> throwDiagnosticError "BUG encountered: impossibly, the symbol we just added vanished"
    Just (SymbolEntry _ _ p) -> return p

checkClassValidity :: ClassName -> Generator ()
checkClassValidity cname = do
  ct <- use classTable
  case lookupClassByName cname ct of
    Nothing -> throwE $ "invalid class " ++ cname ++ " for object " ++ cname
    Just _ -> return ()

{--}

{- Class instances -}
{- Convention: Every generator ensures to clean up its state after itself -}
instance Generatable Program where
  {- Instruction layout for Program:
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
    {- side effects:
       - populate class table
       - add initializers to procedure table
       - increase prefix length -}
    classInstructions <- traverse generator classes
    {- side effects:
       - populate procedure table
       - increase prefix length -}
    procedureInstructions <- traverse (contextGenerator NORMAL) procedures
    let requiredStackMemory = calculateStackMemoryRequirement main
    let stackMemoryAllocationInstructions = replicate requiredStackMemory (PushInt 0)
    prefixLength += requiredStackMemory
    methodTableInstructions <- generateMethodTableInstructions
    prefixLength += length methodTableInstructions
    mainProgramInstructions <- generator main
    return $
      concat classInstructions
        ++ concat procedureInstructions
        ++ stackMemoryAllocationInstructions
        ++ methodTableInstructions
        ++ mainProgramInstructions
        ++ [Halt]

instance Generatable ClassDeclaration where
  {- Instruction layout for ClassDeclaration:
     <code for initializer>
     <code for method 1>
     ...
     <code for method n>
  -}
  generator (Class name parameters mUpperClassName fields initializer methods) = do
    {- side effects:
       - add new empty class table entry for 'name' with field information
       - in case of inheritance, copy relevant information from upper class -}
    classID <- createClassTableEntry name mUpperClassName fields
    {- side effects:
       - add initializer procedure to procedure table
       - increase prefix length -}
    initInstructions <- generateInitializer name parameters initializer
    {- side effects:
       - add methods to corresponding class table
       - increase prefix length -}
    methodInstructions <- traverse (contextGenerator classID) methods
    return $ initInstructions ++ concat methodInstructions

instance ContextGeneratable ClassID MethodDeclaration where
  {- Instruction layout for MethodDeclaration:
        Jump END
        <code for subprocedure 1>
        ...
        <code for subprocedure n>
        <code for stack memory allocation>
        <code for initialization of return parameter>
        <method instruction machine code>
        <return instructions>
   END:
  -}
  contextGenerator classID (Method (ProcedureHeader name parameters mReturnParameter subprocedures) code) = do
    prefixLength += 1
    {- side effects:
       - add method to method table of class "classID"
       - in case of inheritance, override inherited method if necessary -}
    addMethodToClassTable classID name parameters mReturnParameter
    {- save the old procedure table for the reset later -}
    oldProcedureTable <- use procedureTable
    {- side effects:
       - add subprocedures to procedure table
       - increase prefix length -}
    subProcedureInstructions <- traverse (contextGenerator NORMAL) subprocedures
    {- side effects:
       - add parameters to symbol table
       - this includes implicit parameter "this" and return parameter -}
    thisParam <- addMethodParametersToSymbolTable classID parameters mReturnParameter
    {- side effects:
       - increase prefix length -}
    stackMemoryAllocationInstructions <- generateStackMemoryAllocationInstructions parameters mReturnParameter code
    {- side effects:
       - increase prefix length -}
    returnParameterInitInstructions <- generateMethodReturnParameterInitInstructions thisParam parameters mReturnParameter
    {- side effects:
       - increase prefix length -}
    methodInstructions <- generator code
    {- side effects:
       - increase prefix length -}
    returnInstructions <- generateReturnInstructions mReturnParameter
    -- reset symbol table
    symbolTable .= []
    -- cleanup subprocedures from procedure table
    procedureTable .= oldProcedureTable

    newPrefix <- use prefixLength
    return $
      [Jump newPrefix]
        ++ concat subProcedureInstructions
        ++ stackMemoryAllocationInstructions
        ++ returnParameterInitInstructions
        ++ methodInstructions
        ++ returnInstructions

instance ContextGeneratable ProcedureKind ProcedureDeclaration where
  {- Instruction layout for ProcedureDeclaration:
        Jump END
        <code for subprocedure 1>
        ...
        <code for subprocedure n>
        <code for stack memory allocation>
        <code for initialization of return parameter>
        <IF INIT-procedure: code for heap memory allocation>
        <procedure instruction machine code>
        <return instructions>
   END:
  -}
  contextGenerator kind (Procedure (ProcedureHeader name parameters mReturnParameter subprocedures) code) = do
    prefixLength += 1
    {- side effects:
       - add procedure to procedure table -}
    addToProcedureTable name parameters mReturnParameter
    {- save the old procedure table for the reset later -}
    oldProcedureTable <- use procedureTable
    {- side effects:
       - add subprocedures to procedure table
       - increase prefix length -}
    subProcedureInstructions <- traverse (contextGenerator NORMAL) subprocedures
    {- side effects:
       - add parameters to symbol table
       - this includes return parameter -}
    addProcedureParametersToSymbolTable parameters mReturnParameter
    {- side effects:
       - increase prefix length -}
    stackMemoryAllocationInstructions <- generateStackMemoryAllocationInstructions parameters mReturnParameter code
    {- side effects:
       - increase prefix length -}
    returnParameterInitInstructions <- generateProcedureReturnParameterInitInstructions parameters mReturnParameter
    {- side effects:
       - increase prefix length -}
    heapMemoryAllocationInstructions <- generateHeapMemoryAllocationInstructions kind mReturnParameter
    {- side effects:
       - increase prefix length -}
    procedureInstructions <- generator code
    {- side effects:
       - increase prefix length -}
    returnInstructions <- generateReturnInstructions mReturnParameter
    -- reset symbol table
    symbolTable .= []
    -- cleanup subprocedures from procedure table
    procedureTable .= oldProcedureTable

    newPrefix <- use prefixLength
    return $
      [Jump newPrefix]
        ++ concat subProcedureInstructions
        ++ stackMemoryAllocationInstructions
        ++ returnParameterInitInstructions
        ++ heapMemoryAllocationInstructions
        ++ procedureInstructions
        ++ returnInstructions

instance Generatable SyntaxTree.Instruction where
  {- Basic instructions -}
  {- Instruction layout for variable assignment:
      <code for expression>
      LoadStack ...
  -}
  generator (Assignment (NameReference name) expr) = do
    (symPos, symType) <- lookupSymbolPosAndTypeByName name
    exprType <- typify expr
    checkTypeCompatibility exprType symType
    {- side effect:
       - increase prefix length
    -}
    exprInstructions <- generator expr
    prefixLength += 1
    return $
      exprInstructions
        ++ [StoreStack symPos]
  {- Instruction layout for variable assignment:
      LoadStack ...
      <code for expression>
      StoreStack ...
  -}
  generator (Assignment (FieldReference obj field) expr) = do
    (objPos, objType) <- lookupSymbolPosAndTypeByName obj
    (fieldPos, fieldType) <- lookupFieldPosAndTypeByTypeAndFieldName objType field
    exprType <- typify expr
    checkTypeCompatibility exprType fieldType
    prefixLength += 1
    {- side effect:
       - increase prefix length
    -}
    exprInstructions <- generator expr
    prefixLength += 1
    return $
      [LoadStack objPos]
        ++ exprInstructions
        ++ [StoreHeap fieldPos]
  generator (SymbolDeclarationInstruction (IntDeclaration (Int n))) = do
    {- side effect:
       - add new symbol to symbol table
    -}
    pos <- addSymbolToTable n INT
    prefixLength += 2
    return [PushInt 0, StoreStack pos]
  generator (SymbolDeclarationInstruction (ObjectDeclaration (Object cname name))) = do
    checkClassValidity cname
    {- side effect:
       - add new symbol to symbol table
    -}
    pos <- addSymbolToTable name (OBJ cname)
    prefixLength += 2
    return [PushInt (-1), StoreStack pos]
  generator (CallInstruction call) = do
    t <- typify call
    {- side effect:
       - increase prefix length
    -}
    case t of
      Nothing -> generator call
      Just _ -> throwE "type error: we can only fit empty return values here"
  generator (SyntaxTree.Read name) = do
    (pos, t) <- lookupSymbolPosAndTypeByName name
    checkTypeCompatibility (Just INT) t
    prefixLength += 2
    return [MachineInstruction.Read, StoreStack pos]
  generator (SyntaxTree.PrintI expr) = do
    t <- typify expr
    checkTypeCompatibility t INT
    {- side effect:
       - increase prefix length
    -}
    exprInstructions <- generator expr
    prefixLength += 1
    return $ exprInstructions ++ [PrintInt]
  generator (PrintS msg) = do
    prefixLength += 1
    return [PrintStr msg]
  generator (PrintLnS msg) = do
    prefixLength += 1
    return [PrintStrLn msg]
  generator Error = do
    prefixLength += 1
    return [Halt]
  {- Composite instructions -}
  generator (Block oInstructions) = do
    -- reset symbol table afterwards to implement scoping
    oldSymbolTable <- use symbolTable
    {- side effects:
       - increase prefix length
       - add new symbols to symbol table
    -}
    mInstructions <- traverse generator oInstructions
    symbolTable .= oldSymbolTable
    return $ concat mInstructions
  {- Machine instruction layout for if-then-conditionals:
        <code for condition>
        JumpIfFalse END
        <code for body>
    END:
  -}
  generator (IfThen cond body) = do
    {- side effect:
       - increase prefix length
    -}
    condInstructions <- generator cond
    prefixLength += 1
    oldSymbolTable <- use symbolTable
    {- side effects:
       - increase prefix length
       - add new symbols to symbol table
    -}
    bodyInstructions <- generator body
    symbolTable .= oldSymbolTable
    p <- use prefixLength
    return $
      condInstructions
        ++ [JumpIfFalse p]
        ++ bodyInstructions
  generator (While cond body) = do
    oldSymbolTable <- use symbolTable
    start <- use prefixLength
    {- side effect:
       - increase prefix length
    -}
    condInstructions <- generator cond
    prefixLength += 1
    {- side effects:
       - increase prefix length
       - add new symbols to symbol table
    -}
    bodyInstructions <- generator body
    prefixLength += 1
    symbolTable .= oldSymbolTable
    end <- use prefixLength
    return $
      condInstructions
        ++ [JumpIfFalse end]
        ++ bodyInstructions
        ++ [Jump start]

instance Generatable Call where
  generator (SymbolReference (NameReference name)) = do
    pos <- lookupSymbolPosByName name
    prefixLength += 1
    return [LoadStack pos]
  generator (SymbolReference (FieldReference obj field)) = do
    (objPos, t) <- lookupSymbolPosAndTypeByName obj
    fieldPos <- lookupFieldPosByTypeAndFieldName t field
    prefixLength += 2
    return [LoadStack objPos, LoadHeap fieldPos]
  {- Instruction layout for procedure calls with address a and n parameters:
      <code for expression 1>
      ...
      <code for expression n>
      CallProcedure a n
  -}
  generator (Call (NameReference name) actualParameterList) = do
    paramTypes <- traverse typify actualParameterList
    procAddress <- calculateMatchingSetMinimumAddressForProcedureInvocation name paramTypes
    {- side effect:
       - increase prefix length
    -}
    paramInstructions <- traverse generator actualParameterList
    prefixLength += 1
    return $
      concat paramInstructions
        ++ [CallProcedure procAddress (length actualParameterList)]
  {- Instruction layout for method calls with method ID id and n parameters:
      <code for expression 1>
      ...
      <code for expression n>
      CallMethod i n
  -}
  generator (Call (FieldReference objName methodName) actualParameterList) = do
    paramTypes <- traverse typify actualParameterList
    methodID <- calculateMatchingSetMinimumIDForMethodInvocation objName methodName paramTypes
    objPos <- lookupSymbolPosByName objName
    prefixLength += 1
    {- side efect:
       - increase prefix length
    -}
    paramInstructions <- traverse generator actualParameterList
    prefixLength += 1
    return $
      [LoadStack objPos]
        ++ concat paramInstructions
        ++ [CallMethod methodID (length actualParameterList)]

instance Typeable Call where
  typifier (SymbolReference (NameReference name)) = lookupSymbolTypeByNameT name
  typifier (SymbolReference (FieldReference obj field)) = do
    objType <- lookupSymbolTypeByNameT obj
    lookupFieldTypeByTypeAndFieldName objType field
  typifier (Call (NameReference name) actualParameterList) = do
    paramTypes <- traverse typifier actualParameterList
    calculateMatchingSetMinimumTypeForProcedureInvocationT name paramTypes
  typifier (Call (FieldReference obj method) actualParameterList) = do
    paramTypes <- traverse typifier actualParameterList
    calculateMatchingSetMinimumTypeForMethodInvocationT obj method paramTypes

instance Generatable Condition where
  generator (Comparison left relation right) = do
    -- check types
    tLeft <- typify left
    tRight <- typify right
    checkTypeCompatibility tLeft INT
    checkTypeCompatibility tRight INT
    {- side effect:
       - increase prefix length
    -}
    leftInstructions <- generator left
    {- side effect:
       - increase prefix length
    -}
    rightInstructions <- generator right
    prefixLength += 1
    return $
      leftInstructions
        ++ rightInstructions
        ++ [CombineBinary $ conv relation]
    where
      conv SyntaxTree.Equals = MachineInstruction.Equals
      conv SyntaxTree.Smaller = MachineInstruction.Smaller
      conv SyntaxTree.Greater = MachineInstruction.Greater
  generator (Negation cond) = do
    {- side effect:
       - increase prefix length
    -}
    condInstructions <- generator cond
    prefixLength += 1
    return $ condInstructions ++ [CombineUnary Not]

-- this generator expects the expression to be well-typed which needs to be ensured before calling
instance Generatable Expression where
  {- Machine code layout for Expression:
      <code for first term>
      <code for term 2>
      CombinaBinary <sign 2>
      ...
      <code for term n>
      CombineBinary <sign n>
  -}
  generator (Expression ((sign, term) :| signTerms)) = do
    -- If there is a plus, we just generate the factor (works also if s has type OBJ _)
    -- If there is a minus, we generate PushInt 0 before and CombineBinaryMinus after the factor, calculating its negated value
    firstTermInstructions <- case sign of
      SyntaxTree.Plus -> generator term
      SyntaxTree.Minus -> do
        prefixLength += 1
        {- side effect:
           - increase prefix length
        -}
        termInstructions <- generator term
        prefixLength += 1
        return $
          [PushInt 0]
            ++ termInstructions
            ++ [CombineBinary MachineInstruction.Minus]
    {- side effect:
       - increase prefix length
    -}
    signTermsInstructions <- traverse signTermGenerator signTerms
    return $ firstTermInstructions ++ concat signTermsInstructions
    where
      signTermGenerator (sign', term') = do
        termInstructions <- generator term'
        prefixLength += 1
        return $ termInstructions ++ [CombineBinary $ conv sign']
      conv SyntaxTree.Plus = MachineInstruction.Plus
      conv SyntaxTree.Minus = MachineInstruction.Minus

instance Typeable Expression where
  typifier (Expression ((s, t) :| ts)) = do
    ttype <- typifier t
    if null ts && s == SyntaxTree.Plus
      then return ttype
      else do
        -- we just typify all the terms - if there are multiple terms and one is not an integer, the type is invalid
        ttypes <- traverse (typifier . snd) ts
        if isIntType ttype && all isIntType ttypes
          then return $ Just INT
          else throwE "invalid type: non-integer value in algebraic term!"

instance Generatable Term where
  {- Machine code layout for Term:
      <code for first factor>
      <code for factor 2>
      CombinaBinary <operator 2>
      ...
      <code for factor n>
      CombineBinary <operator n>
  -}
  generator (Term factor operatorFactors) = do
    {- side effect:
       - increase prefix length
    -}
    firstFactorInstructions <- generator factor
    {- side effect:
       - increase prefix length
    -}
    otherFactorsInstructions <- traverse otherFactorsGenerator operatorFactors
    return $ firstFactorInstructions ++ concat otherFactorsInstructions
    where
      otherFactorsGenerator (op, factor') = do
        {- side effect:
           - increase prefix length
        -}
        factor'Instructions <- generator factor'
        prefixLength += 1
        return $ factor'Instructions ++ [CombineBinary $ conv op]
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
  generator (CallFactor call) = generator call
  -- for class name 'cname', generateInitializer generates the initializer as a procedure with name "INIT_cname"
  generator (ClassInstantiation cname actualParameterList) = generator (Call (NameReference $ "INIT_" ++ cname) actualParameterList)
  generator (Integer n) = do
    prefixLength += 1
    return [PushInt n]
  generator (CompositeFactor expr) = generator expr

-- these don't introduce much new logic, just syntax tree decomposition for the most part
instance Typeable Factor where
  typifier (CallFactor c) = typifier c
  -- a class instantiation unsurprisingly has the same type as the corresponding initializer/constructor
  typifier (ClassInstantiation cn apl) = typifier (Call (NameReference $ "INIT_" ++ cn) apl)
  typifier (Integer _) = return $ Just INT
  typifier (CompositeFactor e) = typifier e

{--}