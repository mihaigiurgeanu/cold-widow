# cold-widow

Executables and Haskell library to transfer files via QR Codes.

The idea is to generate a list of qr-coedes representing the contents of
the files you want to transfer. The qr-codes will be read as text values by any qr-code
reader supporting alpahnumeric encoding. The texts can be send via any
technology, like email, sms, whatsapp, skype, hangouts, etc to the final
destination. At the final destination, you feed these texts to the decoder
and get the original file structure.

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

To install it in the stack's install directory, type:

    #> stack install

## encode45

The `encode45` utility will get a file as first argument and will output
the encoded text representing the file. The text will contain only characters
allowed by the qr-code alphanumeric mode.

To use it as a qr-code, you need to pass a maximum of about 2900 bytes file to
the `encode45` utility.

## decode45

`decode45` will read from standard output a text containing only the characters
allowed in qr-code alphanumeric mode and will output the corresponding binary file.
The name of the file to which `decode45` will save the binary data _must_ be passed
as the first argument of the `decode45` method.

## write-qr

`write-qr` generates a qrcode pgm image for a file containing only the 45 charcters
allowed by the qr-code alphanumeric mode.

## Usage

To send a number of binary files through qr codes, you first archive the files in
one compressed archive and split it into chunks of 2900 bytes.

For example you could do this:

    #> tar cv mydir/* | bzip2 -9 | split -b 2900 - x

will archive the files in `mydir`, will compress the archive using `bzpi2`, will
split the resulting compressed archived in files of 2900 bytes named starting with
the letter x: `xaa`, `xab`, `xac`, etc.

Encode the binary files into text files accepted by qr-code:

    #> for i in x*; do encode45 $i > $i.txt; done

Generarte qr-codes:

    #> for i in x*.txt; do write-qr $i.txt; done

Now, scan the images with a qr-code scanner and send the text over to the destination
computer.

Run decode for each piece of text:

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
