# Reconciling uncertain cooldown rewards

Cooldown flags record that the cooldown event was raised. They do not prove that every reward effect completed. A reward may contain several effects, and a failure can occur after some effects have already happened.

For configured cooldown rewards, the server log records `Cooldown reward status=attempted` with an occurrence ID, player UUID, site (or `all`), and time. This means the reward listener attempted dispatch; it is not proof of delivery. An asynchronous terminal failure produces a warning with the same context, `outcome=unknown/partial`, and an injection count when available. That count is a diagnostic, not a list of commands or proof that the other effects did not happen. More than one warning can refer to the same occurrence when a configured reward list has multiple failures. A timer failure has a separate correlation ID and may happen before or after an event.

Before considering any manual correction:

1. Preserve the relevant server logs, configuration and reward definitions, and the occurrence ID, UUID, site, and time. Check logs from the server that processed the event.
2. For each configured effect, inspect independent records from the affected system. A failed command can have an uncertain outcome; the log alone cannot establish whether it applied. An unavailable or ambiguous record means the effect remains unknown.
3. After identifying an independently confirmed missing effect, an authorized operator can issue only that effect and record the correction. Do not reset cooldown flags or replay the complete reward merely because a warning appeared: earlier effects may already have completed.

These logs are best-effort diagnostics, not a durable reward journal. An event cancelled by another plugin, disabled reward processing, a deferred reward, an internally caught legacy requirement error, logger failure, shutdown, or a crash can leave no terminal warning. A crash between the flag change and the listener's attempted-dispatch log can leave no occurrence entry at all. Missing logs do not establish success or failure, and a logged attempt does not establish the outcome. Keep external transaction evidence for reconciliation; do not assume exactly-once delivery.
