# Import mongo
from pymongo import MongoClient
# better looking outputs
from pprint import pprint

def connect():	
    """ Method used to conecct to mongo DB """

    db_user = "root"
    db_pass = "rootpass"

    client = MongoClient('mongodb://'+db_user+':'+db_pass+'@192.168.16.3:27017')
    db = client.twitter
    coll = db.tweets_geral
    pprint(db.list_collection_names())
    print("Connected to Mongo")
    return db, coll

        
