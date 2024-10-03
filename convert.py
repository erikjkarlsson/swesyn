#!/usr/bin/python3

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.

# Author: Erik J. Karlsson
# Date:   24/10/3
# 
import xml.etree.ElementTree as ET
import csv
import argparse
import xml.etree.ElementTree as ET
import sqlite3

def write_to_sqlite(word_synonyms, sqlite_file):
    """
    Write swesaurus.xml synonym dictionary to Sqlite databse.

    Sqlite Format: Table: synonyms(word, synonyms)
    Where synonyms is a word list, each word separated by '|'.

    Parameters:
    output_file: Path to output CSV file
    """
    conn = sqlite3.connect(sqlite_file)
    cursor = conn.cursor()

    cursor.execute("DROP TABLE IF EXISTS synonyms")
    cursor.execute("""
    CREATE TABLE synonyms (
        word TEXT PRIMARY KEY,
        synonyms TEXT
    )
    """)

    for word in sorted(word_synonyms.keys()):
        synonyms = "|".join(sorted(word_synonyms[word]))
        cursor.execute(
            "INSERT INTO synonyms (word, synonyms) VALUES (?, ?)",
            (word, synonyms)
        )

    conn.commit()
    conn.close()
    print(f"Data successfully written to {sqlite_file}")

def read_synonyms(input_file) -> {str:{str}}:
    """
    Parses the Swedish synonym XML dataset (swesaurus.xml) a dictionary

    Dictionary format dict(<word> : set(<synonym>, ...) )
    
    Parameters:
    input_file (str): The path to the input swesaurus.xml file.
    """
    word_synonyms = {}

    # Parse the XML file    
    tree         = ET.parse(input_file)
    root         = tree.getroot()    

    # Traverse the XML structure
    for entry in root.findall(".//LexicalEntry"):
        for sense in entry.findall(".//Sense"):
            # Extract the main word for this sense
            word     = sense.attrib.get("id", "").split("..")[0]        
            synonyms = set()
            synonym  = str()

            if not word:
                continue
            
            for relation in sense.findall(".//SenseRelation"):
                for feat in relation.findall(".//feat"):
                    if feat.attrib.get("val") == "syn":
                        synonym = relation.attrib.get("targets", "").split("..")[0]
                        synonyms.add(synonym)
                        
            if synonyms:
                if word in word_synonyms:
                    word_synonyms[word].update(synonyms)
                else:
                    word_synonyms[word] = synonyms

    return word_synonyms

def write_to_csv(word_synonyms, output_file, stdout=False) -> None:
    """
    Write swesaurus.xml synonym dictionary to CSV file.

    CSV header: word, synonyms
    Where synonyms is a word list, each word separated by '|'.

    Parameters:
    output_file: Path to output CSV file
    """

    try:     
        with open(output_file, mode='w', newline='', encoding='utf-8') as csvfile:
            writer = csv.writer(csvfile)

            writer.writerow(["word", "synonyms"])
            if stdout:
                print(f"word, synonyms")

            # Write word / synonym data
            for word in sorted(word_synonyms.keys()):
                synonyms = word_synonyms[word]

                if stdout:
                    print(f"{word}, {'|'.join(sorted(synonyms))}")
                    continue

                writer.writerow([word, "|".join(sorted(synonyms))])
    except:
        RuntimeError(f"Failed to write data for {output_file}")

# Specify the input and output file paths
SWESAURUS_PATH: str  = "swesaurus.xml"
OUTPUT_CSV_PATH: str = "swesaurus.csv"
OUTPUT_SQLITE_PATH: str = "swesaurus.db"

def main() -> None:

    parser = argparse.ArgumentParser(
        description="Parse swesaurus.xml synonym dictionary and convert to a CSV file."
    )
    parser.add_argument(
        "--file", "--input", "-f", type=str,
        default=SWESAURUS_PATH,
        help="Path to the swesaurus.xml swedish synonym dictionary."
    )    
    parser.add_argument(
        "--output", "-o", type=str,
        default=OUTPUT_CSV_PATH,
        help="Path to the output csv."
    )
    parser.add_argument(
        "--print", "-p", type=str,
        default=False,
        help="Print csv data to standard output instead of file."
    )

    parser.add_argument(
        "--sqlite", "--sql", type=None,
        default=False,
        help="Path to sqlite database, convert to sqlite instead of CSV."
    )
    
    args = parser.parse_args()
    syns = read_synonyms(args.file)

    if args.print:
        write_to_csv(syns, args.output, stdout=True)
    else:
        if args.sqlite:
            path = args.sqlite
            write_to_sqlite(syns, path)
            
        else:            
            write_to_csv(syns, args.output)

if __name__ == "__main__":
    main()




