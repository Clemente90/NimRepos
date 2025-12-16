# Evaluation of prior branches for call result binding

I reviewed the four branches in `TMP_NimRepos` that attempted to extend the
`call` instruction with result binding:

* **codex/extend-call-instruction-for-return-values** – Added a register-only
  binding syntax for results and moved return registers into the chosen
  destinations after the call. It also kept result names distinct from argument
  names and excluded bound result registers from the clobber set. However, the
  branch was based on an older revision and would drop many newer tests and
  helper code if merged.
* **codex/extend-call-instruction-for-return-values-4vluow** – Similar to the
  first branch but tightened type checking by threading expected types into
  destination parsing. It still rode on the stale base that removed lots of
  tests and infrastructure.
* **codex/extend-call-instruction-for-return-values-kclnnx** – Required result
  bindings to target the callee’s declared register exactly. The docs were more
  detailed, but the code reverted other recent fixes (label scanning, slot size
  helpers) because of the outdated base revision.
* **codex/extend-call-instruction-for-return-values-re1g7n** – Documented the
  feature clearly and enforced register destinations but also insisted that the
  destination register match the signature. It suffered from the same stale
  baseline and removed many current tests.

**Best ideas chosen:** The most robust technical approach came from the
unsuffixed branch, which allowed call sites to name result bindings and
automatically moved values from the callee’s return registers into the chosen
register destinations while keeping those registers available after the call.
I also adopted the stricter destination-type propagation from the `-4vluow`
branch and the tolerance for parenthesized result declarations from the other
branches.

**Conflicts noted:** Every branch was rebased on an older snapshot that would
delete numerous existing tests and helper routines if merged directly, so I
reimplemented the feature on the current `work` branch instead of cherry-picking
those diffs.
