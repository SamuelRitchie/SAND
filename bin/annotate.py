import os
import sys
import click
import yaml
import logging
import logging.config
from python_scripts.scrapping import annotate

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def setup_logging(default_path='logging.yaml', default_level=logging.INFO, env_key='LOG_CFG'):
    path = default_path
    value = os.getenv(env_key, None)
    if value:
        path = value
    if os.path.exists(path):
        with open(path, 'rt') as f:
            config = yaml.safe_load(f.read())
        logging.config.dictConfig(config)
    else:
        logging.basicConfig(level=default_level)


@click.group()
def cli():
    pass


@click.command(name='annotate-links')
@click.option('--links', default='data/triples.txt')
def annotate_links(links):
    output = annotate(links)
    output.to_csv('data/metadata_dataframe.csv', index=False)


cli.add_command(annotate_links)

if __name__ == '__main__':
    setup_logging()
    cli()
