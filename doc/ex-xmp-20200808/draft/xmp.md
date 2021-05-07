# prerequisite utilities

    npm i -g json

provides a little command line script to wrangle JSON

# xmp.config

exiftool requires that we declare slots in xmp.config:

    ┌─[mengwong@solo-wmw-2] - [~/src/smucclaw/complaw/doc/ex-xmp-20200808/xmp/xmp_pdf] - [2021-03-24 01:29:39]
    └─[0] <git:(xmp 95c1990✱✈) > grep -3 L4json xmp.config
    %Image::ExifTool::UserDefined = (
      'Image::ExifTool::XMP::pdfx' => {
        WRITABLE => 'string', # (default to string-type tags)
        L4json => { }, # stringification of arbitrary JSON
      }
        );

so we're just going to stringify whatever we want to save, under L4json, rather than properly using the XML nature of XMP.



# writing via exiftool

given an actual json structure in inner.json:

    ┌─[mengwong@solo-wmw-2] - [~/src/smucclaw/complaw/doc/ex-xmp-20200808/xmp/xmp_pdf] - [2021-03-24 01:28:51]
    └─[1] <git:(xmp 95c1990✱✈) > cat inner.json
    {
      "BuyerName": "Carol Cucumber",
      "ClosingDate": "1970:01:01 10:00:00Z",
      "Consideration": "USD 100",
      "SellerName": "Alice Apple",
      "Potato": { "fr": "pomme de terre",
                  "en": "spud" }
    }
    
we stringify it:

    ┌─[mengwong@solo-wmw-2] - [~/src/smucclaw/complaw/doc/ex-xmp-20200808/xmp/xmp_pdf] - [2021-03-24 01:28:27]
    └─[0] <git:(xmp 95c1990✱✈) > json -e 'console.log (JSON.stringify({"L4json":JSON.stringify(this)}))' < inner.json | head -1 | tee new.json
    {"L4json":"{\"BuyerName\":\"Carol Cucumber\",\"ClosingDate\":\"1970:01:01 10:00:00Z\",\"Consideration\":\"USD 100\",\"SellerName\":\"Alice Apple\",\"Potato\":{\"fr\":\"pomme de terre\",\"en\":\"spud\"}}"}

to `new.json`:

    ┌─[mengwong@solo-wmw-2] - [~/src/smucclaw/complaw/doc/ex-xmp-20200808/xmp/xmp_pdf] - [2021-03-24 01:31:29]
    └─[0] <git:(xmp 95c1990✱✈) > cat new.json
    { "L4json": "{\"BuyerName\":\"Carol Cucumber\",\"ClosingDate\":\"1970:01:01 10:00:00Z\",\"Consideration\":\"USD 100\",\"SellerName\":\"Alice Apple\",\"Potato\":{\"fr\":\"pomme de terre\",\"en\":\"spud\"}}"
    }

and we save it to the PDF:

    ┌─[mengwong@solo-wmw-2] - [~/src/smucclaw/complaw/doc/ex-xmp-20200808/xmp/xmp_pdf] - [2021-03-24 01:31:38]
    └─[0] <git:(xmp 95c1990✱✈) > exiftool -config xmp.config -j+=new.json plain.pdf
        1 image files updated

then we confirm it comes back out:

    ┌─[mengwong@solo-wmw-2] - [~/src/smucclaw/complaw/doc/ex-xmp-20200808/xmp/xmp_pdf] - [2021-03-24 01:27:05]
    └─[0] <git:(xmp 95c1990✱✈) > exiftool -j plain.pdf | json -a | json 'L4json' | json
    {
      "BuyerName": "Carol Cucumber",
      "ClosingDate": "1970:01:01 10:00:00Z",
      "Consideration": "USD 100",
      "SellerName": "Alice Apple",
      "Potato": {
        "fr": "pomme de terre",
        "en": "spud"
      }
    }


