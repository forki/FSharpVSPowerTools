namespace FSharp.Control

open System
open System.Threading

module Observable =
    let ofSeq<'TItem>(items:'TItem seq) =
        { new IObservable<_> with
            member __.Subscribe(observer:IObserver<_>) =
                for item in items do observer.OnNext item      
                observer.OnCompleted()     
                { new IDisposable with member __.Dispose() = () }
        }

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