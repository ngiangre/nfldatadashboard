
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nfldatadashboard

## Goal

Interactively compare NFL player’s performance within positions. Only
QB, WR, TE, and RB from Next Gen Stats 2023 season using {nflreadr}.

## Target Outcomes

1.  ~~Select and compare player performance within positions~~
2.  ~~Visualize the distribution of player performance within
    positions~~
3.  List custom player ranking within positions

## Questions

1.  ~~How does my select player compare to other players within and
    across position’s performance statistics?~~
2.  ~~How do players perform overall across performance statistics?~~
3.  What are player’s overall performance for select statistics?

## Design

- 4 navbar tabs for each position
  - Sidebar: Position statistics selection and descriptions - will be
    passed to plots below
  - Select tab
    - Players selection
    - Season-average heatmap
    - Season-wide boxplots - interactive
  - Explore tab
    - Players selection
    - Season-wide scatterplot - interactive
    - Season-average vs. variance scatterplot - interactive

## Data

<u>Two types of player statistics:</u>

1.  Season-wide (each week)
2.  Season-average (average over weeks)

<u>Three positions:</u>

1.  Quarterback (QB)
2.  Wide receiver (WR)
3.  Tight End (TE)
4.  Running back (RB)

<u>Performance statistics:</u>

1.  QB: 18
2.  WR: 12
3.  TE: 12
4.  RB: 11
