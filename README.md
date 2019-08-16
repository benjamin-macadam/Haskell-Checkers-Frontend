# Table of Contents

1.  [System](#org44273b0)
    1.  [Human move](#org879b0c7)
    2.  [CPU move](#org7d7b8d1)
    3.  [End of Game](#orgf97fc2d)
2.  [Note for Students](#org026505e)


<a id="org44273b0"></a>

## System

The general idea is a separation of concerns. There is the "tuistate",
and this includes the gamestate. The gamestate is only accessed when:

-   Adding checkers to the board
-   Making a move

There are two "modes" for the TUI.


<a id="org879b0c7"></a>

## Human move

The human controls the cursor and builds a move. This is activated
when the gamestate has a "Human" player making a move.

-   pressing the arrow keys navigates the board
-   pressing the space key adds the current square to the move.
-   pressing enter "applies the move".
