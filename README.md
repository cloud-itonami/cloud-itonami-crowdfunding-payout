# cloud-itonami-crowdfunding-payout

**PayoutAdvisor ⊣ PayoutGovernor** — the actor that computes what a
creator is owed and never moves a yen of it.

A `cloud-itonami` blueprint actor in the workspace's standard shape:
advisor sealed into one node of a [langgraph-clj](https://github.com/kotoba-lang/langgraph)
StateGraph, an independent governor, a 0→3 phase gate, an append-only
audit ledger. Domain rules from
[`kotoba-lang/crowdfunding`](https://github.com/kotoba-lang/crowdfunding);
design record: ADR-2607268500.

## Why crowdfunding payout is not marketplace payout

A marketplace seller ships and is then paid. A crowdfunding creator is
paid and *then* ships, months later, using the money to do it. Paying
100% on collection maximises the creator's ability to build and maximises
the backer's exposure; paying nothing until delivery makes the campaign
pointless.

So this actor does not pick a side — it makes the choice **explicit and
auditable** as a tranche schedule the backer can read before pledging,
and it releases each tranche only against a gate derived from records
this actor actually holds.

## Gates are derived, never asserted

`store/gate-facts` computes which release gates are met from the
fulfilment lines and the collection result. `:on-production-start` is
**never inferred** — production having begun is not observable from
fulfilment states, and inferring it would invent the very fact the
holdback exists to require. A gate an advisor asserts is not a gate.

## The invariant

> The governor rejects; the actor never writes what it refuses.

Seven HARD checks, un-overridable by any human approval:

| Check | Why |
|---|---|
| Plan not conserved | fees + net must equal what backers were charged, and tranches must equal the net — checked here *and* in the library, because a rounding leak is silent |
| Destination unverified | re-derived from the **store**; found at execution it is a payment already in flight |
| Gate not met | re-derived from fulfilment records |
| Out of order | a later tranche cannot jump an earlier one, or the ordering backers read was decorative |
| Held tranche | only a named human's `resolve-hold` crosses that line |
| `:effect` ≠ `:propose` | a claim to actuate outside governance |
| Scope exclusion | claiming to have paid out, transferred or refunded |

Five ops **always escalate**, in every phase including 3:
`:bind-payout-destination` (where a creator's money goes — the
money-equivalent of issuing an identity), `:release-tranche` (when it
leaves — computing is reversible, releasing is not), `:hold-tranche`
(stopping someone's livelihood), `:plan-refund` (who eats the processing
fees a processor will not return), `:flag-payout-concern`.

## Dust goes to the creator, soonest

`pay.core/split-allocations` routes rounding dust to the first entry. In
a marketplace that means the seller rather than the operator; here the
first entry is the *earliest* tranche, so dust reaches the creator sooner
rather than sitting in a holdback nobody remembers to release.

## No custody

Nothing here moves money, holds a key, or reaches the network.
`:payout/custodial? false` is stamped on every plan.
`kotoba-lang/pay`'s zero-custody boundary is preserved all the way up; a
rail adapter performs transfers after a human approves them, outside this
actor.

## Run it

```bash
kbb -M:dev:run     # offline demo: a conserving plan, refusals, a human-gated release, a refund's true cost
kbb -M:dev:test    # 28 tests
kbb -M:lint
```

## Licence

AGPL-3.0-or-later.
