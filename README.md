[![Anaconda-Server Badge](https://anaconda.org/bioconda/skmer/badges/version.svg)](https://anaconda.org/bioconda/skmer)
[![Anaconda-Server Badge](https://anaconda.org/bioconda/skmer/badges/downloads.svg)](https://anaconda.org/bioconda/skmer)

# Skmer 2
## !! This GitHub is still under construction, please report any bugs, or contact edcharvel@ucsd.edu for any questions!!

For the original Skmer, please visit https://github.com/shahab-sarmashgh/Skmer.git

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
4. Clone the github repository by running (or you can download the repo)
```
    git clone https://github.com/echarvel3/Skmer-2.git
```
Using Skmer
------------
### reference
usage:
```
python ./skmer/__main__.py reference ref_dir -r path_to_reference -o output_prefix -l library_name
```
The reference in question can be a genome assembly, a jellyfish .hist file that comes from a genome assembly, or a RESPECT estimate of a repeat spectrum with an estimate of genome size concatenated as the last row.
