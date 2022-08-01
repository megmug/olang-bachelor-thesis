module MachineInstruction where

-- A code address represents an index into the code segment of the machine
type CodeAddress = Int

-- A stack address represents an index (relative the the current base address) into the machine's stack
type StackAddress = Int

-- A heap address represents an index into the machine's heap
type HeapAddress = Int

-- A field index represents an index into an object's fields on the heap
type FieldIndex = Int

type ClassID = Int

type MethodID = Int

data Instruction
  = LoadStack StackAddress
    {- Push value from specified address (relative to stack frame base) to stack
       Requires: Address is a valid relative address
       Ensures: Value is pushed onto the stack
    -}
  | StoreStack StackAddress
    {- Pop value from top of stack and store it to specified address (relative to stack frame base)
       Requires: Address is a valid relative address
       Ensures: - Old top of stack value is popped from the stack
                - Old top of stack value is stored at specified stack address
    -}
  | PushInt Integer
    {- Push specified integer onto the stack
       Ensures: Specified integer is pushed onto the stack
    -}
  | LoadHeap FieldIndex
    {- LoadHeap i: Push value from heap address stored at top of stack at index i
       Requires: Top of stack represents a valid heap address (called a)
       Ensures: - Old top of stack value is popped from the stack
                - Value at heap address a and heap frame index i is pushed onto the stack
    -}
  | StoreHeap FieldIndex
    {- StoreHeap i: Store value from top of stack to heap address saved below the top at field i
       Requires: Second top stack element is a valid heap address (called a)
       Ensures: - Old top and second top elements are popped from the stack
                - Old top of stack value is now stored at heap address a at index i
    -}
  | AllocateHeap Int ClassID
    {- AllocateHeap n t: Create heap allocation of size n and with associated type t and return the address to top of stack
       Requires: t represents a valid type - which means that a method table with index t must exist
       Ensures: - New heap frame of size n is allocated and initialized:
                  -- Type is initialized
                  -- All data fields of the frame are initialized with zero values
                - Address of newly created heap frame is pushed onto the stack
    -}
  | CreateMethodTable ClassID [(MethodID, CodeAddress)]
    {- CreateMethodTable id [(0, a0), (1, a1), ..., (n, an)] creates a method table for class with ID id that notes method 0 at code address a0, method 1 at code address a1 etc.
       Requires: Method table for class ID id does not yet exist
       Ensures: - Method table with specified class ID id is created
                - Methods 0 to an are added to the method table with their respective entries
    -}
  | Jump CodeAddress
    {- Jump a: Unconditionally jump to code address a
       Requires: a is a valid code address
       Ensures: Program counter is overridden with address a
    -}
  | JumpIfFalse CodeAddress
    {- Jump to specified code address a iff top of stack is False
       Requires: a is a valid code address
       Ensures: - Old top of stack is popped
                - IFF old top of stack represents boolean value "False": Program counter is overridden with address a
                - IFF old top of stack represents boolean value "True": Program continues normally
    -}
  | CallProcedure CodeAddress Int
    {- CallProcedure a n calls the procedure at address a with n parameters
       Requires: - a is a valid code address
                 - The top n elements of the old stack represent the parameters of the procedure
       Ensures: - The n parameter values are popped from the stack
                - A new stack frame is pushed onto the stack, consisting of the return address and the old stack frame base address
                - The n parameters are pushed onto the stack into the new stack frame in order
                - Current program counter is saved as return address in the new stack frame
                - Current base address is saved in the new stack frame
                - Program jumps to procedure with address a
                - Base address register is overridden with new base address
    -}
  | CallMethod MethodID Int
    {- CallMethod i n calls the method of the parameter object with index i and n additional parameters
       Requires: - The top n + 1 elements of the old stack represent the parameters of the method
                 - The first of the n + 1 elements is the address a of the object whose method is called
                 - The object a has a method with index i
       Ensures: - The n + 1 parameter values are popped from the stack
                - A new stack frame is pushed onto the stack, consisting of the return address and the old stack frame base address
                - The n + 1 parameters are pushed onto the stack into the new stack frame in order
                - Current program counter is saved as return address in the new stack frame
                - Current base address is saved in the new stack frame
                - Program jumps to method i of object a
                - Base address register is overridden with new base address
    -}
  | Return Bool
    {- Return b: Return from the procedure, jumping back to the return address, popping the current stack frame and, depending on b, possibly returning a value
       Requires: - Stack frame has a valid return address a and stack frame base address b
                 - Depending on b, return value may be needed on top of stack
       Ensures: - Program jumps to a
                - Current stack frame is popped
                - Stack frame base address is overridden with b
                - IFF b represents boolean value "True": Old top of stack is interpreted as return value and is pushed onto the new stack
    -}
  | CombineUnary UnaryOperator
    {- CombineUnary op: Combine element at top of stack with respect to op
       Requires: Top element of stack is valid argument for op
       Ensures: - Top element of stack is popped
                - Result of combining is pushed onto the stack
    -}
  | CombineBinary BinaryOperator
    {- CombineBinary op: Combine two elements at top of stack with respect to op
       Requires: Top two elements of stack are valid arguments for op
       Ensures: - Top two elements of stack are popped
                - Result of combining both arguments is pushed onto the stack
    -}
  | Read
    {- Read integer from input
       Requires: Machine receives a valid integer non-deterministically
       Ensures: Input integer is pushed onto the stack
    -}
  | PrintInt
    {- Write integer to output
       Ensures: - Top of stack is popped
                - Integer is written to output
    -}
  | PrintStr String
    {- PrintStr s: Print s to output -}
  | PrintStrLn String
    {- PrintStrLn s: Print s to output with a newline at the end -}
  | Halt
    {- Halt the machine, writing message to output -}
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