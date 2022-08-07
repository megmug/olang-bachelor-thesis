module MachineInstruction where

-- A code address is an index into the code segment of the machine
type CodeAddress = Int

-- A stack address is an index (relative the the current base address) into the machine's stack
type StackAddress = Int

-- A heap address is an index into the machine's heap
type HeapAddress = Int

-- A field index is an index into an object's fields on the heap
type FieldIndex = Int

type ClassID = Int

type MethodID = Int

data Instruction
  = {-- Core machine instructions --}
    PushInt Integer
  | LoadStack StackAddress
  | StoreStack StackAddress
  | CombineUnary UnaryOperator
  | CombineBinary BinaryOperator
  | Jump CodeAddress
  | JumpIfFalse CodeAddress
  | Read
  | PrintInt
  | PrintStr String
  | PrintStrLn String
  | Halt
  | {-- Instructions for procedure support --}
    CallProcedure CodeAddress Int
  | Return Bool
  | {-- Instructions for support of object-oriented features --}
    LoadHeap FieldIndex
  | StoreHeap FieldIndex
  | AllocateHeap Int ClassID
  | CreateMethodTable ClassID [(MethodID, CodeAddress)]
  | CallMethod MethodID Int
  deriving (Eq, Show)

data UnaryOperator = Not deriving (Eq, Show)

data BinaryOperator
  = Equals
  | Smaller
  | Greater
  | Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Show)