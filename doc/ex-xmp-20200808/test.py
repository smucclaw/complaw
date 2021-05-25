import pikepdf

pdf = pikepdf.open('fruit-contract-2.pdf')

with pdf.open_metadata() as meta:
    meta['l4:buyername'] = 'Wong'
    meta['l4:consideration'] = {
        'date': '20/05',
        'amount': 200
    }

pdf.save('fruit-contract-3.pdf')