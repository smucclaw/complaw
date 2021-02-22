import json
from libxmp import XMPFiles, consts

def read_xmp(file_pdf = ''):
    xmp_file = XMPFiles(file_path = file_pdf, open_forupdate = True)

    # Extract XMP
    xmp = xmp_file.get_xmp()

    return xmp

def write_xmp(file_pdf = '', file_json = ''):
    '''
    Opens an PDF file and JSON file for writing JSON into XMP.

    Parameters
        file_pdf (str) : Location of the PDF file
        file_json (str) : Location of the JSON file
    '''
    xmp_file = XMPFiles(file_path = file_pdf, open_forupdate = True)

    # Extract XMP
    xmp = xmp_file.get_xmp()

    # Insert XMP
    xmp = insert_xmp(xmp, file_json)

    print(xmp)
    # Put into file
    xmp_file.put_xmp(xmp)
    xmp_file.close_file()

def get_json(xmp):
    pass

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

if __name__ == '__main__':
    write_xmp(file_pdf = 'docs/gdoc4.pdf', file_json = 'docs/alt2.json') 
