
outer/inner ip


logical 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
|outer.valid() outer.dst inner.dst | outer.valid() outer.dst out |
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     1            3          *          -             -       4
	 0            *         99          1            3        4



physical 1

++++++++++++++++++++++++++++++++++++++++++++++++++++++++   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 outer.valid() outer.dst | outer.valid() outer.dst out | ; | outer.valid() inner.dst | outer.valid() outer.dst out|
++++++++++++++++++++++++++++++++++++++++++++++++++++++++   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     1             3           -            -       4           0              3          1             3       4



physical 2
++++++++++++++++++++++++++++++++++++++++++++++++++++++++   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 outer.valid() inner.dst | outer.valid() outer.dst out | ; | outer.valid() outer.dst | outer.valid() outer.dst out|
++++++++++++++++++++++++++++++++++++++++++++++++++++++++   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     0            99            1           3       4            1            3            -            -       4
