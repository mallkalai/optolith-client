type t = {
  id: int,
  name: string,
  effect: string,
  range: string,
  duration: string,
  target: string,
  traditions: Ley_IntSet.t,
  src: list(SourceRef.t),
  errata: list(Erratum.t),
};

module Decode = {
  open Json.Decode;

  type tL10n = {
    id: int,
    name: string,
    effect: string,
    range: string,
    duration: string,
    target: string,
    src: list(SourceRef.t),
    errata: list(Erratum.t),
  };

  let tL10n = json => {
    id: json |> field("id", int),
    name: json |> field("name", string),
    effect: json |> field("effect", string),
    range: json |> field("range", string),
    duration: json |> field("duration", string),
    target: json |> field("target", string),
    src: json |> field("src", SourceRef.Decode.list),
    errata: json |> field("errata", Erratum.Decode.list),
  };

  type tUniv = {
    id: int,
    traditions: list(int),
  };

  let tUniv = json => {
    id: json |> field("id", int),
    traditions: json |> field("traditions", list(int)),
  };

  let t = (univ, l10n) => (
    univ.id,
    {
      id: univ.id,
      name: l10n.name,
      effect: l10n.effect,
      range: l10n.range,
      duration: l10n.duration,
      target: l10n.target,
      traditions: Ley_IntSet.fromList(univ.traditions),
      src: l10n.src,
      errata: l10n.errata,
    },
  );

  let all = (yamlData: Yaml_Raw.yamlData) =>
    Yaml_Zip.zipBy(
      Ley_Int.show,
      t,
      x => x.id,
      x => x.id,
      yamlData.blessingsUniv |> list(tUniv),
      yamlData.blessingsL10n |> list(tL10n),
    )
    |> Ley_IntMap.fromList;
};