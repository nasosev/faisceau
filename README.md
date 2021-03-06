# _faisceau_

**Cellular sheaves for F#.**

---
## Warning

This project is currently in very early development and should not be used.

## About

_faisceau_ is an experimental toolkit for computing with cellular sheaves in F#, inspired by Michael Robinson's [PySheaf](https://github.com/kb1dds/pysheaf).

We are presently in very early development and most of the intended functionality is not implemented.

### Usage

Look at the Jupyter Notebooks in the [examples](examples) folder.

For simplicity and performance, _faisceau_ works with coefficients in the binary field F2.

### Dependencies

- [XPlot.Plotly](https://fslab.org/XPlot/) is needed for plotting.
- [FsCheck](https://fscheck.github.io/FsCheck/) is needed for testing.

### References

[1] CURRY, Justin. Sheaves, cosheaves and applications. _arXiv preprint arXiv:1303.3255_, 2013. <https://arxiv.org/abs/1303.3255>

[2] GHRIST, Robert W. _Elementary applied topology_. Seattle: Createspace, 2014. <https://www.math.upenn.edu/~ghrist/notes.html>

[3] CURRY, Justin; GHRIST, Robert; NANDA, Vidit. Discrete Morse theory for computing cellular sheaf cohomology. _Foundations of Computational Mathematics_, 2016, 16.4: 875-897. <https://arxiv.org/abs/1312.6454v2>

[4] HANSEN, Jakob; GHRIST, Robert. Toward a Spectral Theory of Cellular Sheaves. _arXiv preprint arXiv:1808.01513_, 2018. <https://arxiv.org/abs/1808.01513>

---

## Roadmap

### Features

- Many more examples.
- Six operations.
- Integrate FSharpPlus [categorical abstractions](https://fsprojects.github.io/FSharpPlus/abstractions.html).

### Optimisations

- Implement 'Scythe' algorithm from Curry/Ghrist/Nanda.
- Improve performance overall.

---

## License

[MIT License](../master/LICENSE). Copyright (c) 2019 Nasos Evangelou-Oost.
