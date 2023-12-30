
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nfldatadashboard

<!-- badges: start -->
<!-- badges: end -->

## Goal

Interactively compare NFL player’s performance within positions. Only
QB, WR, and RB from Next Gen Stats 2023 season using {nflreadr}.

## Outcomes

1.  Select and compare player performance within positions
2.  Visualize the distribution of player performance within positions
3.  List custom player ranking within positions

## Questions

1.  How does my select player compare to other(s) across position’s
    performance statistics?
2.  How do players perform overall across performance statistics?
3.  What are player’s overall performance for select statistics?

## Design

- 3 navbar tabs for each position
  - Sidebar: Position statistics selection - will be passed to plots
    below
  - Select tab
    - Sidebar: Players selection
    - Season-average heatmap
    - Season-wide boxplot
  - Explore tab
    - Sidebar: Players selection (updated from plot interactivity too)
    - Season-wide scatterplot - use ggiraph for interactivity
    - Season-average histogram - add vertical lines for selected players

## Data

<u>Two types of player statistics:</u>

1.  Season-wide (each week)
2.  Season-average (average over weeks)

<u>Three positions:</u>

1.  Quarterback (QB)
2.  Running back (RB)
3.  Wide receiver (WR)

<u>Performance statistics:</u>

1.  QB: 18
2.  RB: 11
3.  WR: 12
