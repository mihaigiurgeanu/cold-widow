# cold-widow

Executables and Haskell library to transfer files via QR Codes.

The idea is to generate a list of qr-coedes representing the contents of
the files you want to transfer. The qr-codes will be read as text values by any qr-code
reader supporting alpahnumeric encoding. The texts can be send via any
technology, like email, sms, whatsapp, skype, hangouts, etc to the final
destination. At the final destination, you feed these texts to the decoder
and get the original file structure.

When generating qr-codes, the application will archive and compress the files
you want to send, then will encode the resulting stream of octets as text, using
the 45 characters allowed in the alpha-numeric mode of the qr-codes.

The application will split the content you want to transfer in as many qr-codes
as it is needed to transfer the entire content.

## Installation

The only supported installation method, right now, is from source files,
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
    #> cold-widow mk-codes [<options>...] [<file_or_folder>...]
    #> cold-widow mc [<options>...] [<file_or_folder>...]

The `make-codes` command is used to generate the qr-codes that encode the content
of files and/or entire folders with the purpose of transfering them to another device
by scanning the generated qr-codes. 

Synonyms of the `make-codes` command are:

*   `mk-codes`
*   `mc`
*   `qr`

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

If no files or folders are specified on the command line, then the application will read
and generate qr-codes for the content read from stanard input. In this case, the
_2 steps execution_ will be different, in that, at the first run will display on
standard error the command line switches that should be used to set the proper block
size. You should run the program a second time, with the new command line arguments
to obtain the qr-codes.

### receive

    #> cold-widow receive [<option>....] [<file>....]
	#> cold-widow rcv [<option>...] [<file>....]
	#> cold-widow r [<option>...] [<file>....]

`cold-widow receive` command will read the contents of the files specified in the
command line, or of the standard output if no file is specified. Those files should
contain the text obtained when scanning the qr-codes generated by `cold-widow make-codes`
command. We will refer to the text obtained from reading a qr-code as the _encoded text_.
The `receive` command is the inverse of the `make-codes` command.

There are more synonyms for the `receive` command, and you can use any of them instead
of it:

*   `rcv`
*   `r`

When receiving the transfered files, you may either save every _encoded text_ in a separate
file and pass all the resulting files to `cold-widow receive` command, or copy all the 
_encoded texts_ in one file, separated by at least one empty line, then pass this file to
the `cold-widow receive` command. Of course, you may choose to use a combination of the
methods described above. You can also send the _encoded texts_ to the `cold-widow receive`
command through the standard input of the command, by not specifying a filename on the
command line.

The accepted options are:

*   `-o <dir>`, `--output-dir <dir>` - use the specified directory to output the files. If
    the specified directory does not exist, cold-widow will create it for you.
*   `-a`, `--archived`, `--keep-archive` - do not automatically unarchive the files. In
    this case, the archive will be output at the standard output of the command.
*   `-z`, `--compressed` - keep the compressed archive, do not automatically uncompress the
    archive containing the transfered files. In this case, the compressed archive will be
	written at the standard output of the command.

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

## Create a qr codes video with ffmpeg


The following is taken from [ffmpeg documentation](http://ffmpeg.org/ffmpeg.html):

> You can extract images from a video, or create a video from many images:
> For extracting images from a video:
>
>     ffmpeg -i foo.avi -r 1 -s WxH -f image2 foo-%03d.jpeg
>
> This will extract one video frame per second from the video and will output them in files
> named foo-001.jpeg, foo-002.jpeg, etc. Images will be rescaled to fit the new WxH values.
>
> If you want to extract just a limited number of frames, you can use the above command in 
> combination with the -frames:v or -t option, or in combination with -ss to start extracting
> from a certain point in time.
>
> For creating a video from many images:
>
>     ffmpeg -f image2 -framerate 12 -i foo-%03d.jpeg -s WxH foo.avi
>
> The syntax foo-%03d.jpeg specifies to use a decimal number composed of three digits padded
> with zeroes to express the sequence number. It is the same syntax supported by the C 
> printf function, but only formats accepting a normal integer are suitable.
>
> When importing an image sequence, -i also supports expanding shell-like wildcard patterns
> (globbing) internally, by selecting the image2-specific -pattern_type glob option.
>
> For example, for creating a video from filenames matching the glob pattern foo-*.jpeg:
>
>     ffmpeg -f image2 -pattern_type glob -framerate 12 -i 'foo-*.jpeg' -s WxH foo.avi
