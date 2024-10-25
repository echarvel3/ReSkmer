[![Anaconda-Server Badge](https://anaconda.org/bioconda/skmer/badges/version.svg)](https://anaconda.org/bioconda/skmer)
[![Anaconda-Server Badge](https://anaconda.org/bioconda/skmer/badges/downloads.svg)](https://anaconda.org/bioconda/skmer)

# Skmer 2

Installation
------------
**On 64-bit Linux and Mac OSX**, you can install Skmer from bioconda channel using conda package manager. 
1. Install [Miniconda][4] (you can skip this if you already have either of Miniconda or Anaconda installed). 
2. Add the bioconda channel by running the following commands in your terminal (order matters):
```
    conda config --add channels defaults
    conda config --add channels bioconda
    conda config --add channels conda-forge
```
3. Run the following command to install Skmer (and all dependencies) 
```
    conda install skmer
```

**Alternatively, and for all other OS and architectures**, you can download the github repository and install Skmer using the setup script. 
1. You need to have python 2.7 or later installed
2. Install [Jellyfish][2] (v2.2.6 or later), [Mash][3] (v2.3 or later), and [seqtk][5] (v1.3), and add the path to
 their binary to the system path (so you can, e.g., run `jellyfish --version`, `mash --version`, and `seqtk` successfully in the terminal). 
3. Clone the github repository by running (or you can download the repo)
```
    git clone https://github.com/shahab-sarmashghi/Skmer.git
```
4. Change to the Skmer directory and run
```
    python setup.py install
```

Using Skmer
------------
### reference
usage:
```
python ./skmer/__main__.py reference ref_dir -r ${path-to-reference}-o output_prefix
```
The reference in question can be a genome assembly, a jellyfish .hist file that comes from a genome assembly, or a RESPECT estimate of a repeat spectrum with an estimate of genome size concatenated as the last row.
