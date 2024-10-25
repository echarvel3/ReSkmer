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
Skmer has five sub-commands:

### reference
Gets the path to a directory of FASTQ/FASTA files (one uncompressed *.fastq/.fq/.fa/.fna/.fasta* file per each sample) and creates a reference library containing the estimates of sequencing parameters as well as the Mash sketch for each sample. If you have paired-end sequencing reads, we suggest using tools such as [BBMerge][10] to merge overlapping read pairs. If the input is an assembled sequence (determined by the length of sequences) the correction for low coverage and sequencing error is not applied to that sample. All corrected pairwise genomic distances are then estimated and written to a file. For a test run, change the directory to `data` under your Skmer installation directory, and run
```
skmer reference ref_dir -p 4
```
The genome-skims and assemblies in `ref_dir` directory are processed (using 4 cores in parallel), and a reference `library` is created in the working directory. You can specify a custom name (and so its path) for your library using `-l` option
```
skmer reference ref_dir -l custom_library_name
```
Default k-mer size is set to `31` which is the maximum length allowed by Mash, and can be changed using `-k` option. We do not recommend using k-mers smaller than ~`21`, as k-mers without any shared evolutionary history start to seem similar just out of random chance. The sketch size can also be changed using `-s` option from its default value `10000000`. Decreasing the sketch size will reduce the size of library on disk, but also compromises the accuracy of distance estimation. The corrected pairwise distances are estimated and written to the file `ref-dist-mat.txt` in the working directory by default. The output prefix can be changed using `-o` option
```
skmer reference ref_dir -r ${path-to-reference}-o output_prefix
```
The reference in question can be a genome assembly, a jellyfish .hist file that comes from a genome assembly, or a RESPECT estimate of a repeat spectrum with an estimate of genome size concatenated as the last row.
