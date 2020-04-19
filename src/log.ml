(** Terminal and file logging with {!Timber}.

    @see <https://github.com/glennsl/timber> for usage information *)

let with_namespace = Timber.Log.withNamespace
let perf = Timber.Log.perf

module type Logger = Timber.Logger
