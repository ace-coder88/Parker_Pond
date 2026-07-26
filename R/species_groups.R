# Species-group presets for the Shiny sidebar (common-name patterns).

species_group_choices <- c(
  "All birds" = "all",
  "Owls" = "owls",
  "Warblers" = "warblers",
  "Woodpeckers" = "woodpeckers",
  "Sparrows" = "sparrows",
  "Thrushes" = "thrushes",
  "Finches" = "finches",
  "Flycatchers" = "flycatchers",
  "Vireos" = "vireos",
  "Chickadees" = "chickadees",
  "Ducks" = "ducks",
  "Hawks" = "hawks",
  "Custom" = "custom"
)

species_group_ids <- setdiff(unname(species_group_choices), c("all", "custom"))

#' Return species names matching a group id, intersected with `species`.
match_species_group <- function(species, group) {
  species <- unique(as.character(species))
  species <- species[!is.na(species) & nzchar(species)]
  if (length(species) == 0 || is.null(group) || !nzchar(group)) {
    return(character(0))
  }

  if (identical(group, "thrushes")) {
    hit <- grepl("Thrush|Robin|Bluebird|Veery", species, ignore.case = TRUE) &
      !grepl("Waterthrush", species, ignore.case = TRUE)
    return(sort(species[hit]))
  }

  if (identical(group, "hawks")) {
    hit <- grepl(
      "Hawk|Eagle|Osprey|Kestrel|Merlin|Falcon|Harrier",
      species,
      ignore.case = TRUE
    ) &
      !grepl("Nighthawk", species, ignore.case = TRUE)
    return(sort(species[hit]))
  }

  pattern <- switch(
    group,
    owls = "\\bOwl\\b",
    warblers = "Warbler|Redstart|Ovenbird|Northern Parula|Yellowthroat|Waterthrush",
    woodpeckers = "Woodpecker|Flicker|Sapsucker",
    sparrows = "Sparrow|Junco|Towhee",
    finches = "Finch|Goldfinch|Siskin|Crossbill|Grosbeak|Cardinal",
    flycatchers = "Flycatcher|\\bPhoebe\\b|Pewee|\\bKingbird\\b",
    vireos = "Vireo",
    chickadees = "Chickadee|Titmouse|Nuthatch",
    ducks = "Duck|Mallard|Teal|Merganser|Goldeneye|Bufflehead|Wigeon|Pintail|Scaup|Shoveler|Eider",
    NULL
  )

  if (is.null(pattern)) {
    return(character(0))
  }

  sort(species[grepl(pattern, species, ignore.case = TRUE, perl = TRUE)])
}

#' Infer which radio group matches an exact selected set (or all/custom).
infer_species_group <- function(selected, all_species) {
  if (is.null(selected)) {
    selected <- character(0)
  }
  selected <- unique(as.character(selected))
  selected <- selected[!is.na(selected) & nzchar(selected)]

  if (length(selected) == 0) {
    return("all")
  }

  for (group in species_group_ids) {
    expected <- match_species_group(all_species, group)
    if (length(expected) > 0 && setequal(selected, expected)) {
      return(group)
    }
  }

  "custom"
}

species_group_label <- function(group) {
  idx <- match(group, unname(species_group_choices))
  if (is.na(idx)) {
    return(group)
  }
  names(species_group_choices)[[idx]]
}
