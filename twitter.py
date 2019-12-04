
import os
import tweepy
import datetime
from mongo_db import connect
from pprint import pprint
import time

try:
    base, coll = connect()
except:
    print("Could not connect to MongoDB")
    exit()

 

consumerKey = os.getenv("CONSUMER_KEY")
consumerSecret = os.getenv("CONSUMER_SECRET")
accessToken = os.getenv("ACCESS_TOKEN")
accessTokenSecret = os.getenv("ACCESS_TOKEN_SECRET")

auth = tweepy.OAuthHandler(consumerKey, consumerSecret)
auth.set_access_token(accessToken, accessTokenSecret)
api = tweepy.API(auth, wait_on_rate_limit=True)
places = api.geo_search(query="Brazil", granularity="country")
place_id = places[0].id

status = api.rate_limit_status()
public_tweets = tweepy.Cursor(api.search, q='-filter:retweets lang:pt since:2019-11-08', until = "2019-11-09", tweet_mode='extended').items(5000)

data = datetime.datetime(2019, 11, 12)

for tweet in public_tweets:
    author = tweet.user.screen_name
    text = tweet.full_text 
    date = tweet.created_at.strftime("%Y-%m-%d")

    if not coll.tweets_geral.find_one(filter={'text' : text}) and not base.tweets_lula.find_one(filter={'text' : text}) and not base.tweets_bolsonaro.find_one({'text' : text}):
        coll.insert_one({"author": author, "text": text, "date": date})

