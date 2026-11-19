"""Exports bivario's named bivariate corner palettes as 7x7 hex grids.

Run with the venv python (see build_palettes.R): the R side no longer uses
reticulate for this -- reticulate binds one Python interpreter per R
session, so once anything earlier in a long build script has initialized a
different Python, use_python() can't switch it without restarting R. This
script is invoked as a plain subprocess instead, decoupled from whatever
Python (if any) reticulate has already bound to.

Output: JSON {palette_name: [hex, hex, ...]} (49 colors per palette, flat,
in R's expand.grid(a = values_a, b = values_b) order -- a fastest, b
slowest). build_palettes.R reshapes+transposes this back into the same n x m
matrix the original reticulate-based code produced (verified against the
already-published bivario.* palettes in sysdata.rda).
"""

import sys
import json
import itertools
import numpy as np
from bivario import NamedBivariateColourmap
from bivario.palettes import BIVARIATE_CORNER_PALETTES

n, m = 7, 7
values_a = np.linspace(0, 1, n)
values_b = np.linspace(0, 1, m)

# a fastest, b slowest -- matches R's expand.grid(a = values_a, b = values_b)
pairs = list(itertools.product(values_b, values_a))
grid_b = np.array([p[0] for p in pairs])
grid_a = np.array([p[1] for p in pairs])

out = {}
for name in BIVARIATE_CORNER_PALETTES.keys():
    cmap = NamedBivariateColourmap(name)
    rgb_array = np.round(np.asarray(cmap(values_a=grid_a, values_b=grid_b)) * 255).astype(int)
    out[name] = ["#%02X%02X%02X" % (r, g, b) for r, g, b in rgb_array]

with open(sys.argv[1], "w") as f:
    json.dump(out, f)
