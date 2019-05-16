open Types

module type ZFS = sig

  type context

  type context_or_fail = (context,fsErr) Core.result

  val step : context -> context_or_fail

  val apply_command : context -> context_or_fail Async.Deferred.t

  val init_context : unit -> context

  val get_zipper : context -> fsZipper

  val add_command : fsCommand -> context -> context

  val remove_command : context -> context
end

module ZFS : ZFS 

module TxZFS : ZFS 