# _faisceau_

**Cellular sheaves for F#.**

---

## ABOUT

Faisceau is an experimental toolkit for computing with cellular sheaves in F#, inspired by Michael Robinson's [PySheaf](https://github.com/kb1dds/pysheaf).

We are presently in very early development and most of the intended functionality is not implemented.

For simplicity, coefficients are taken in the binary field F2.

### Usage

Look at the Jupyter Notebooks in the [examples](examples) folder. (TODO)

### Dependencies

- [XPlot.Plotly](https://fslab.org/XPlot/) is needed for plotting.
- [FsCheck](https://fscheck.github.io/FsCheck/) is needed for testing.

### References

[1] CURRY, Justin. Sheaves, cosheaves and applications. _arXiv preprint arXiv:1303.3255_, 2013. <https://arxiv.org/abs/1303.3255>

[2] GHRIST, Robert W. _Elementary applied topology_. Seattle: Createspace, 2014. <https://www.math.upenn.edu/~ghrist/notes.html>

[3] CURRY, Justin; GHRIST, Robert; NANDA, Vidit. Discrete Morse theory for computing cellular sheaf cohomology. _Foundations of Computational Mathematics_, 2016, 16.4: 875-897. <https://arxiv.org/abs/1312.6454v2>

---

## TODO

### Bugs

- Chain product not correctly implemented, Chain type needs redesign.

### Features

- Many examples.
- Cosheaves.
- Local cohomology.
- Six operations.
- Integrate FSharpPlus [categorical abstractions](https://fsprojects.github.io/FSharpPlus/abstractions.html).

### Optimisations

- Improve performance.

---

## LICENSE

[MIT License](../master/LICENSE). Nasos Evangelou-Oost, 2019.
