# Startup failure and shutdown

VotingPlugin may be unloaded after a partial startup. Shutdown therefore treats each optional subsystem independently:
a failed close does not prevent the remaining queues, reminders, cooldown checks, signs, handlers, or executors from being
cleaned up. The vote timer uses a bounded termination wait, forces cancellation if it does not terminate, and restores
the interrupt flag when shutdown is interrupted. Repeated unload callbacks are safe after the first unload completes.
