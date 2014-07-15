namespace FSharp.Control

open System
open System.Threading

type private TestObserver<'a>() =
    let mutable stopped = false
    let elements = new System.Collections.Generic.List<'a>()
    interface IObserver<'a> with
        member x.OnNext value = elements.Add(value)
        member x.OnError e = raise e
        member x.OnCompleted () = stopped <- true

    member x.Elements = elements
    member x.Stopped = stopped

module Observable =
    let ofSeq<'TItem>(items:'TItem seq) =
        { new IObservable<_> with
            member __.Subscribe(observer:IObserver<_>) =
                for item in items do observer.OnNext item      
                observer.OnCompleted()     
                { new IDisposable with member __.Dispose() = () }
        }

    /// Very naive implementation - TODO: try to find better version
    let toSeq (observable: IObservable<'T>) =
        let o = TestObserver<'T>()
        use sub = observable.Subscribe o

        while not o.Stopped do
            System.Threading.Thread.Sleep(10)
        o.Elements

    let singleton x =
        { new IObservable<_> with
            member __.Subscribe(observer:IObserver<_>) =
                observer.OnNext x
                observer.OnCompleted()
                { new IDisposable with member __.Dispose() = () }
        }

[<AutoOpen>]
module AsyncExtensions =
    type Microsoft.FSharp.Control.Async with
        /// Starts the specified operation using a new CancellationToken and returns
        /// IDisposable object that cancels the computation. This method can be used
        /// when implementing the Subscribe method of IObservable interface.
        static member StartDisposable(op:Async<unit>) =
            let ct = new CancellationTokenSource()
            Async.Start(op, ct.Token)
            { new IDisposable with 
                member x.Dispose() = ct.Cancel() }