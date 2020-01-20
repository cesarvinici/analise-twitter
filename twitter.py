
import os
import tweepy
import datetime
from mongo_db import connect
from pprint import pprint
import time

""" 
This .py script will read twittes from twitter and save on a mongodb table.
This was made when ex president Lula was release from jail and the role purpose of this was trying to see what people were saying on twitter about this.

So two key-words was used:
"Lula" wich relates to the ex president.
"Bolsonaro" the name of the actual president (they basically hate each other, and so their followers)
I also did a search with no key-words to see what people were saying on general

"""

try:
    base, coll = connect()
except:
    print("Could not connect to MongoDB")
    exit()

 
# Get the twitter auth keys from a .env file 
consumerKey = os.getenv("CONSUMER_KEY")
consumerSecret = os.getenv("CONSUMER_SECRET")
accessToken = os.getenv("ACCESS_TOKEN")
accessTokenSecret = os.getenv("ACCESS_TOKEN_SECRET")

# connect on twitter
auth = tweepy.OAuthHandler(consumerKey, consumerSecret)
auth.set_access_token(accessToken, accessTokenSecret)
api = tweepy.API(auth, wait_on_rate_limit=True)
# search for only portuguese/Brazilian tweets
places = api.geo_search(query="Brazil", granularity="country")
place_id = places[0].id

status = api.rate_limit_status()
# filtering by date and language with no retweets
public_tweets = tweepy.Cursor(api.search, q='-filter:retweets lang:pt since:2019-11-08', until = "2019-11-09", tweet_mode='extended').items(5000)

# interate over the tweets the extract some info and check if it already exists on table
for tweet in public_tweets:
    author = tweet.user.screen_name
    text = tweet.full_text 
    date = tweet.created_at.strftime("%Y-%m-%d")

    if not coll.tweets_geral.find_one(filter={'text' : text}) and not base.tweets_lula.find_one(filter={'text' : text}) and not base.tweets_bolsonaro.find_one({'text' : text}):
	# save on mongo db 
        coll.insert_one({"author": author, "text": text, "date": date})

