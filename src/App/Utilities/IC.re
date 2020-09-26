/**
 * The Improvement Cost for an entry with a numeric value.
 */
type t =
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H;

/**
 * Get the IC-specific multiplier for calculating AP cost.
 *
 * This is the exact AP cost value for adding (or removing) spells and
 * liturgical chants as well as the improvement cost value for skills up to
 * SR 12 and attributes up to 14.
 */
let%private getAPCostBaseByIC = ic =>
  switch (ic) {
  | A => 1
  | B => 2
  | C => 3
  | D => 4
  | E => 5
  | F => 8
  | G => 10
  | H => 20
  };





/**
 * Returns the AP cost for a single SR with a specific IC.
 */
let%private getCost = (ic, sr) =>
  getAPCostBaseByIC(ic) * sr;

/**
 * Returns the AP cost between the defined lower and upper SR. The AP cost for
 * the lower bound are not included, as they would have been already paid or you
 * would not get the AP to get to that same SR.
 */
let%private getAPForBounds = (ic, l, u) =>
  Ix.range((l + 1, u))
  |> List.fold_right(sr => getCost(ic, sr) |> (+), _, 0);

/**
 * `getAPRange ic fromSR toSR` returns the AP cost for the given SR range.
 */
[@gentype]
let getAPForRange = (ic, fromSR, toSR) =>
  fromSR < toSR
    ? getAPForBounds(ic, fromSR, toSR)
    : fromSR > toSR ? - getAPForBounds(ic, toSR, fromSR) : 0;

[@gentype]
let getAPForInc = (ic, fromSR) => getCost(ic, fromSR + 1);

[@gentype]
let getAPForDec = (ic, fromSR) => - getCost(ic, fromSR);

[@gentype]
let getAPForActivatation = getAPCostBaseByIC;

/**
 * Returns the name of the passed Improvement Cost.
 */
[@gentype]
let icToStr = ic =>
  switch (ic) {
  | A => "A"
  | B => "B"
  | C => "C"
  | D => "D"
  | E => "E"
  | F => "F"
  | G => "G"
  | H => "H"
  };

/**
 * Returns an index used for getting the IC-based cost for an Activatable entry.
 */
[@gentype]
let icToIx = ic =>
  switch (ic) {
  | A => 0
  | B => 1
  | C => 2
  | D => 3
  | E => 4
  | F => 5
  | G => 6
  | H => 7
  };

module Decode = {
  open Json.Decode;

  let t = json =>
    json
    |> string
    |> (
      x =>
        switch (x) {
        | "A" => A
        | "B" => B
        | "C" => C
        | "D" => D
        | "E" => E
        | "F" => F
        | "G" => G
        | "H" => H
        | _ => raise(DecodeError("Unknown improvement cost: " ++ x))
        }
    );
};
