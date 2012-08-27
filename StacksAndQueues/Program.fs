open System

//purely functional stack
type Stack<'a> =
    | EmptyStack
    | Node of 'a * Stack<'a> 

    member this.Push element =
        Node(element, this)
    member this.Pop() =
        match this with
        | Node(element, tail) -> element, tail
        | EmptyStack -> failwith "Can't pop from empty stack"

let processInput() =
    let stack = EmptyStack
    Seq.unfold
        (fun _ -> let input = Console.ReadLine()
                  if input <> "" then Some (input, ()) else None)
        ()
    |> Seq.fold 
        (fun (stack : Stack<_>) input -> 
            if input = "-" then 
                let x, newStack = stack.Pop()
                printfn "%s" x
                newStack
            else stack.Push(input))
        stack

//oop-style stack
type Node<'a>(element : 'a) =
    let mutable item = element
    member public this.Item with get() = item and set(v) = item <- v
    member val public Next : Node<'a> option = None with get, set
type StackOOP<'b>() =
    let mutable first : Node<_> option = None

    member this.Push(element : 'b) =
        let node = new Node<_>(element)
        node.Next <- first
        first <- Some node
    member this.Pop() =
        match first with
        | None -> failwith "Can't pop from empty stack"
        | Some x -> 
            first <- x.Next
            x.Item

let processInputOOP() =
    let stackOfStrings = new StackOOP<string>()
    Seq.unfold
        (fun _ -> let input = Console.ReadLine()
                  if input <> "" then Some (input, ()) else None)
        ()
    |> Seq.iter (fun input -> if input = "-" then stackOfStrings.Pop() |> printfn "%s"
                                             else stackOfStrings.Push(input))  

[<EntryPoint>]
let main _ =
   printfn """If you want to use purely functional stack, type "fp"."""
   if Console.ReadLine().ToLower() = "fp" then processInput() |> ignore else processInputOOP()    
   0