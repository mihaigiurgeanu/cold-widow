# cold-widow

Executables and Haskell library to transfer files via QR Codes.

The idea is to generate a list of qr-coedes representing the archived
version folder. The qr-codes will be read as text values by any qr-code
reader supporting alpahnumeric encoding. The texts can be send via any
technology, like email, sms, whatsapp, skype, hangouts, etc to the final
destination. At the final destination, you feed these texts to the decoder
and get the original file structure.

## Installation

The only supported installation method is from source files,
[stack](http://www.haskellstack.org/).

## Building from source

### Prerequisites

You will need to [download and install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

You also need [git](https://git-scm.com/) to get the latest source code.

### Get the source

    git clone https://github.com/mihaigiurgeanu/cold-widow.git
    
### Building

To build the project, you need first to run `stack setup`. This command
will make sure you have the correct haskell compiler, and, if you don't
have it, it will download and install one in a separate location in such
a way to not interract with your existing haskell environment (if you have one):

    #> cd /the/location/of/cold-widow/
    #> stack setup
    
After the setup (you only need to run setup once) you may build, test or install
the software. To build, simply issue:

    #> stack build
    
To run the tests:

    #> stack test
    
To install it in the stack's install directory, type:

    #> stack install
    
## Usage

There are 3 utilities delivered with cold-widow:

*   `cold-widow`
*   `encode45`
*   `decode45`

The `cold-widow` is the main executable that converts the files to send into qr-codes
and then, reconverts the scanned qr-codes values back into the original files.

`enccode45` and `decode45` deals only with encoding and decoding a file to/from 
text containing only the 45 characters allowed by qr-code alphanumeric mode. This
text is used as a convenience transport since it is efficientely encoded into qr-codes
and allows copy paste between common applications.

## cold-widow

The `cold-widow` utility has two main use cases:

1.  Converting the files to send into qr-codes.
2.  Converting the texts resulted from scanning the qr-codes
    back to the original files.
    
In the first case, the `cold-widow` utility will automatically archive and compress
the files to send. In the second case, `cold-widow` will reconstruct the compressed
archive, will uncompress and unarchive the files using their original names.

To run cold-widow you use this syntax:

    #> cold-widow <command> [<option>...]
    
The commands that you can pass to the `cold-widow` executable are:

*   make-codes or mk-codes or mc or qr - used to generate qr-codes
*   receive or rcv or r - used to restore the original files

### make-codes

    #> cold-widow make-codes [<options>...] [<file_or_folder>...]

The `make-codes` command is used to generate the qr-codes for some files. Synonyms of
the `make-codes` command are:

*   mk-codes
*   mc
*   qr
    
The options accepted by `cold-widow make-codes` command are:

*   `-1`, `--fast`, `--single-step` - do the encoding in a single step, on the expense 
    of using more memory. See bellow the _2 step execution_.
*   `-t`, `--temporary-file` - do the encoding in a single step, using a temporary file. 
    See the discussion bellow about the _2 step execution_.
*   `-s <n>`, `--bs <n>`, `--block-size <n>` - use <n> bytes as the block-size, do not
    compute the size automatically.
*   `-q <n>`, `--qr-version <n>` - use the version n of the qr-codes (n can be from
    1 to 40).
*   `-l <level>`, `--error-level <level>` - use the error correction level specified
    by `<level>` which can be one of the letters `L`, `M`, `Q`, `H`. The default is
    to automatically determine the error level correction.

The error levels are:

*   `L` - error recovery up to 7%
*   `M` - error recovery up to 15%
*   `Q` - error recovery up to 25%
*   `H` - error recovery up to 30%


Normal execution of `cold-widow` is a _2 steps execution_:

1.  In the first step, the application computes the optimum block size by
    archiving and compressing the files on the command line to find out
    the total size of compressed archive then splitting the size into
    equal block sizes so the minimum number of blocks to be used.
    
2.  Generating the qr-codes by lazily archiving and compressing the files.

This _2 steps execution_ assures that the minimum amount of memory will be used. To
speed up things, you may specify the `-1` switch or the equivalent synonyms, 
`--fast`, `--single-step`, to instruct the application to compute the archive
in memory then generate the qr-codes.

The `-t` (`--temporary-file`) switch is the same as `-1` switch, instructing the 
application to not use the _2 steps execution_, but instead of composing the archive in
memory as `-1` switch does, save the archive as a temporary file.

## encode45

The `encode45` utility will get a file as first argument and will output
the encoded text representing the file. The text will contain only characters
allowed by the qr-code alphanumeric mode.

To use it as a qr-code, you need to pass a maximum of about 2900 bytes file to
the `encode45` utility.

## decode45

`decode45` will read from standard output a text containing only the characters
allowed in qr-code alphanumeric mode and will decoded as a binary file. The name
of the file to which `decode45` will save the binary data _must_ be passed as
the first argument of the `decode45` method.

## Manual encode/decode to/from alphanumeric files

You can use `encode45` and `decode45` commands to manually encode/decode to/from
text using characters supported by alphanumeric qr codes. In the following example
we take the case where you hava some files you want to encode to qr code alphanumerio 
characters using `encode45`. To do this, you first archive the files using `tar`,
then compress the archive using `bzip2` and finally split the compressed archived
in files of 2900 bytes. This is because a single qr-code cannot hold more then 
approximately 2900 (maybe a bit more) bytes.

The command:

    #> tar cv informic-0.1.0/*.patch | bzip2 -9 | split -b 2900 - informic-0.1.0/x
    
will archive the files with the extension `.patch` located in the `informic-0.1.0/`
folder, will compress the archive using `bzpi2` utility, will split the resulting
compressed archived in files named `xaa`, `xab`, `xac`, etc. of 2900 bytes each
and will put these files into `informic-0.1.0/` folder.

To encode those files using _cold-widow's encode45_ you could use the following:

    #> cd informic-0.1.0
    #> for i in x*; do encode45 $i > $i.txt; done 
    
Then you should use a qr-code generator to generate one qr-code for each
`xaa.txt`, `xab.txt`, `xac.txt`, etc files generated by the above commands. Scan
the qr-codes with you mobile phone and copy-paste the text into a email message
that you can send to anyone you want.

Finally, using `decode45` you can convert the fragments of text back to the original
archive. Copy in the clipboard the text coresponding to first part (the file `xaa`
in the example above) and paste it in a file, for example in the `xaa.txt` file:

    #> decode45 xaa < xaa.txt

This will generate on disk the file named `xaa` with the same contents of the 
original `xaa` file which is a part of the splited compressed archive. After
doing this for all file parts, you can use the following to obtain the original
files structure:

    #> cat x* | bzcat | tar xv
