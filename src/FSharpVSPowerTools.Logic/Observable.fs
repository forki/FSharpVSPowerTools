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