#if INTERACTIVE
#r @"..\packages\NUnit.2.6.1\lib\nunit.framework.dll"
#endif

open System
open System.Collections.Generic

open NUnit.Framework

type UnionFind =
    abstract member Union : int * int -> UnionFind
    abstract member Find  : int * int -> bool

type QuickFind(N) =
    let id = [| for i in 0..N-1 -> i |]

    interface UnionFind with
        member this.Union(p, q) =
            let prev = id.[p]
            for idx in 0..N-1 do
                if id.[idx] = prev then id.[idx] <- id.[q]
            this :> UnionFind

        member this.Find(p, q) = (id.[p] = id.[q])

type QuickUnion(N) =
    let id = [| for i in 0..N-1 -> i |]

    member private this.Root(idx) =
        if id.[idx] = idx then idx else this.Root(id.[idx])

    interface UnionFind with
        member this.Union(p, q) =
            id.[this.Root(p)] <- id.[this.Root(q)]
            this :> UnionFind

        member this.Find(p, q) = (this.Root(p) = this.Root(q))

type QuickUnionBalanced(N) =
    let id = [| for i in 0..N-1 -> i |]
    let sizes = [| for _ in 0..N-1 -> 1 |]

    member private this.Root(idx) =
        if id.[idx] = idx then idx else this.Root(id.[idx])

    interface UnionFind with
        member this.Union(p, q) =
            let proot, qroot = this.Root(p), this.Root(q)
            if sizes.[proot] < sizes.[qroot] 
                then id.[proot] <- qroot; sizes.[q] <- sizes.[q] + sizes.[p]
                else id.[qroot] <- proot; sizes.[p] <- sizes.[p] + sizes.[q]
            this :> UnionFind

        member this.Find(p, q) = (this.Root(p) = this.Root(q))

type QuickUnionBalancedWithPathCompression(N) =
    let id = [| for i in 0..N-1 -> i |]
    let sizes = [| for _ in 0..N-1 -> 1 |]

    member private this.Root(idx) =
        if id.[idx] = idx then 
            idx
        else 
            id.[idx] <- id.[id.[idx]] //one-pass path compression
            this.Root(id.[idx])

    interface UnionFind with
        member this.Union(p, q) =
            let proot, qroot = this.Root(p), this.Root(q)
            if sizes.[proot] < sizes.[qroot] 
                then id.[proot] <- qroot; sizes.[q] <- sizes.[q] + sizes.[p]
                else id.[qroot] <- proot; sizes.[p] <- sizes.[p] + sizes.[q]
            this :> UnionFind

        member this.Find(p, q) = (this.Root(p) = this.Root(q))

let performanceComparison() =
    let measurePerf f = 
        let watch = System.Diagnostics.Stopwatch.StartNew()
        f()
        watch.Stop()
        watch.ElapsedMilliseconds
    
    let size = 10000
    let rand = new Random()
    let get() = [| for i in 0..100000 -> rand.Next(size), rand.Next(size) |] 
    let formSequence, findSequence = get(), get()

    let run formS findS (uf : UnionFind) =
        for p, q in formS do uf.Union(p, q) |> ignore
        for p, q in findS do uf.Find(p, q)  |> ignore
    
    let qf  = measurePerf <| fun() -> run formSequence findSequence (new QuickFind(size))
    let qu  = measurePerf <| fun() -> run formSequence findSequence (new QuickUnion(size))
    let qub = measurePerf <| fun() -> run formSequence findSequence (new QuickUnionBalanced(size))
    let quc = measurePerf <| fun() -> run formSequence findSequence (new QuickUnionBalancedWithPathCompression(size))
    printfn "Got:\n\t%i64 for QuickFind\n\t%i64 for QuickUnion\n\t%i64 for QuickUnionBalanced \
            \n\t%i64 for QuickUnionBalancedWithPathCompression"
            qf qu qub quc

printfn "Working..."
performanceComparison()

//Working...
//Got:
//	193764 for QuickFind
//	128164 for QuickUnion
//	1764 for QuickUnionBalanced 
//	1164 for QuickUnionBalancedWithPathCompression

// tests

[<TestFixture>]
type Tests() =
    static member private UnionFindTest(uf : UnionFind) =
        uf.Union(3, 4).Union(3, 8).Union(6, 5).Union(9, 4).Union(2, 1) |> ignore
        Assert.True(uf.Find(8, 9))
        Assert.False(uf.Find(0, 7))
        uf.Union(6, 1).Union(7, 2).Union(0, 1).Union(5, 0) |> ignore
        Assert.True(uf.Find(7, 0))
    
    [<Test>]
    static member test() =
        Tests.UnionFindTest(new QuickFind(10))
        Tests.UnionFindTest(new QuickUnion(10))
        Tests.UnionFindTest(new QuickUnionBalanced(10))
        Tests.UnionFindTest(new QuickUnionBalancedWithPathCompression(10))