#!/usr/bin/env python
from optparse import OptionParser as OP
import os, glob

__version__ = "0.0.1"

usage = """usage: %prog [options] texheader texfiles...

Compiles each texfile given as a tex document, using texheader as the 
preambule. Outputs eps with tight bounding boxes. Requires latex, ps2eps
and dvips, and epstopdf for pdf output.

    By Gael Varoquaux"""

parser = OP(usage=usage, version="%prog " +__version__ )
options, args = parser.parse_args()

if len(args)<2:
    print usage
    sys.exit(1)

tex_header_string = file(args[0]).read()

tex_files = args[1:]

for file_name in tex_files:
    print >> file("tex_to_eps_file.tex", "w"), r"""%s
    \begin{document}
    %s
    \end{document}
    """ % (tex_header_string, file(file_name).read() )
    #os.system("latex -interaction scrollmode tex_to_eps_file")
    os.system("latex tex_to_eps_file")
    os.system("dvips -Ppdf -G0 tex_to_eps_file.dvi")
    os.system("ps2eps -f tex_to_eps_file.ps")
    os.environ['GS_OPTIONS'] = "-dUseFlatCompression=true -dPDFSETTINGS=/prepress -sColorImageFilter=FlateEncode -sGrayImageFilter=FlateEncode -dAutoFilterColorImages=false -dAutoFilterGrayImages=false -dEncodeColorImages=false -dEncodeGrayImages=false -dEncodeMonoImages=false"
    os.system("epstopdf --nocompress tex_to_eps_file.eps")
#    os.system("pdflatex tex_to_eps_file")
#    os.system("gs -q -sDEVICE=bbox -dBATCH -dNOPAUSE tex_to_eps_file.pdf")
    try:
        os.rename("tex_to_eps_file.pdf", file_name[:-4]+".pdf")
    except OSError:
        pass

map(os.unlink, glob.glob("tex_to_eps_file.*")) 

