# Mangrel

Usage:

Compile with:
```bash
stack build
```

There are two modes available.
The first mode takes a number of clusters and a source image:

```bash
stack run 5 warhol.jpg
```

The second mode takes a cluster value and two source images.
The first image is recolored using the palette of the second.

```bash
stack run 5 matisse.jpg vangogh.jpg
```

All source images must be in the img folder.
The program will output to out.png by default.
