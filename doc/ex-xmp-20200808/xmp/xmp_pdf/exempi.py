import json
import xmltodict
from libxmp import XMPFiles, consts

def read_xmp(file = ''):
    xmp_file = XMPFiles(file_path = file, open_forupdate = True)

    # Extract XMP
    xmp = xmp_file.get_xmp()
    json = xmltodict.parse(xmp)

    out = json.dumps(json)
    return out

def write_xmp(pdf = '', json = ''):
    '''
    Opens an PDF file and JSON file for writing JSON into XMP.

    Parameters
        pdf (str) : Location of the PDF file
        json (str) : Location of the JSON file
    '''
    xmp_file = XMPFiles(file_path = pdf, open_forupdate = True)

    # Extract XMP
    xmp = xmp_file.get_xmp()

    # Insert XMP
    xmp = insert_xmp(xmp, json)

    print(xmp)
    # Put into file
    xmp_file.put_xmp(xmp)
    xmp_file.close_file()

def insert_xmp(xmp, file_json, encode = consts.XMP_NS_PDFX):
    '''
    Insert XMP into PDF

    Parameters
        xmp : XMP
        contract (Dict) : The JSON file
        encode : Metadata specification
    '''
    with open(file_json, 'r') as file:
        contract = json.load(file)

    for key in contract:
        # Get value
        value = contract[key]

        # Get value type
        value_type = type(value)

        if value_type == int:
            xmp.set_property_int(encode, key, value)
        elif value_type == float:
            xmp.set_property_float(encode, key, value)
        elif value_type == bool:
            xmp.set_property_bool(encode, key, value)
        else:
            xmp.set_property(encode, key, value)
    
    return xmp

# if __name__ == '__main__':
#     write_xmp(file_pdf = 'docs/gdoc4.pdf', file_json = 'docs/alt2.json') 
