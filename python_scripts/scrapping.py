import pandas as pd
import urllib
import bs4
import logging.config
from tqdm import tqdm
tqdm.pandas()
logger = logging.getLogger(__name__)


def scrapping_jazz_metadata(url):
    try:
        request_text = urllib.request.urlopen(url).read()
        page = bs4.BeautifulSoup(request_text, "lxml")

        name = page.find('title').getText().split('About: ')[1]

        if page.find('span', {'property': 'dbo:background'}) is not None:
            background = page.find('span', {'property': 'dbo:background'}).getText()
        else:
            background = None

        if page.find('span', {'property': 'dbo:birthDate'}) is not None:
            birth_year = page.find('span', {'property': 'dbo:birthDate'}).getText()[0:4]
        else:
            birth_year = None

        if page.findAll('a', {'rel': 'dbo:birthPlace'}):
            birth_place = [p.getText().split('dbr:')[1] for p in page.findAll('a', {'rel': 'dbo:birthPlace'})]
        else:
            birth_place = None

        if page.find('span', {'property': 'dbo:deathDate'}) is not None:
            death_year = page.find('span', {'property': 'dbo:deathDate'}).getText()[0:4]
        else:
            death_year = None

        if page.find('a', {'rel': 'dbo:deathPlace'}) is not None:
            death_place = page.find('a', {'rel': 'dbo:deathPlace'}).getText().split('dbr:')[1]
        else:
            death_place = None

        if page.findAll('a', {'rel': 'dbo:genre'}):
            genre = [p.getText().split('dbr:')[1] for p in page.findAll('a', {'rel': 'dbo:genre'})]
        else:
            genre = None

        instrument = []
        if page.findAll('a', {'rel': 'dbo:instrument'}):
            instrument.extend([p.getText().split('dbr:')[1] for p in page.findAll('a', {'rel': 'dbo:instrument'})])
        if page.findAll('span', {'property': 'dbp:instrument'}):
            instrument.extend([p.getText() for p in page.findAll('span', {'property': 'dbp:instrument'})])
        if page.findAll('a', {'rel': 'ns1:hypernym'}):
            instrument.extend([p.getText().split('dbr:')[1] for p in page.findAll('a', {'rel': 'ns1:hypernym'})])
        if page.find('span', {'property': 'dbp:occupation'}) is not None:
            instrument.append(page.find('span', {'property': 'dbp:occupation'}).getText())

        if 'Musician' in instrument:
            instrument.remove('Musician')

        if page.findAll('a', {'rel': 'dbo:associatedMusicalArtist'}):
            associated_artists = [p.getText().split('dbr:')[1] for p in
                                  page.findAll('a', {'rel': 'dbo:associatedMusicalArtist'})]
        else:
            associated_artists = None

        if page.findAll('a', {'rel': 'dbo:associatedBand'}):
            associated_bands = [p.getText().split('dbr:')[1] for p in page.findAll('a', {'rel': 'dbo:associatedBand'})]
        else:
            associated_bands = None

        result = {'Name': name, 'Background': background, 'Birth Year': birth_year, 'Birth Place': birth_place,
                  'Death Year': death_year, 'Death Place': death_place, 'Genre': genre, 'Instruments': instrument
            , 'Associated Artists': associated_artists, 'Associated Bands': associated_bands}

    except:
        result = {}

    return result


def annotate(csv="data/triples.txt"):
    logger.info('fetching metadata...')
    data = pd.read_csv(csv, sep=" ", header=None, names=['Artiste1', 'Lien', 'Artiste2', '.'])
    unic_jazzmen = pd.DataFrame(data.Artiste1.append(data.Artiste2).drop_duplicates(), columns=['artist_url'])
    unic_jazzmen['metadata'] = unic_jazzmen.artist_url.progress_apply(scrapping_jazz_metadata)
    unic_jazzmen_dataframe = pd.concat([unic_jazzmen.artist_url, unic_jazzmen.metadata.apply(pd.Series)],axis=1).\
        reset_index(drop=True)

    return unic_jazzmen_dataframe
