#!/usr/bin/python3


import os
import sys
import json
import random
import re
import string
import html

usersdir = "home"
parcelfileprefix = ".cadastre"

backgroundchars = ",,..''``\"" + ' '*100

# backslash which escapes the screen and enters your brain?
regex = '[^0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&\'()*+,-./:;<=>?@[\\\\\\]^_`{|}~ ]'
replacechars = re.compile(regex)

htmlwrapper="<html><head><meta charset='utf-8'><link type='style/css'></link></head><body><pre>%s</pre></body></html>"

def hashPlace(x, y):
    return str(x) + "," + str(y)

def parsePlace(place):
    return [int(x) for x in place.split(',')]


class Cadastre:
    
    def __init__(self, name, parcelfile, width, height):
        self.name = name
        self.towndatafile = os.path.join(sys.path[0], name+".json")
        self.parcelwidth = width
        self.parcelheight = height
        self.parcelfile = parcelfile
        if os.path.exists(self.towndatafile):
            with open(self.towndatafile) as f:
                towndata = json.load(f)
                self.places = towndata["places"]
                self.owners = towndata["owners"]
        else:
            self.places = {}
            self.owners = {}
    
    def update(self):
        
        for user in os.listdir(usersdir):
            parcel = os.path.join(usersdir, user, parcelfileprefix, self.parcelfile)
            if os.path.isfile(parcel):
                self.loadParcel(user, parcel)
    
    def loadParcel(self, user, parcel):
        try:
            with open(parcel) as f:
                x, y = [int(x) for x in f.readline().strip().split(' ')]
                place = hashPlace(x, y)
                if place not in self.places or self.places[place]["owner"] == None or self.places[place]["owner"] == user:
                    lines = [replacechars.sub(' ',line)[:self.parcelwidth] for line in f][:self.parcelheight]
                    for i in range(self.parcelheight):
                        if i >= len(lines):
                            print(i)
                            lines.append(' '*self.parcelwidth)
                        elif len(lines[i]) < self.parcelwidth:
                            lines[i] += ' '*(self.parcelwidth-len(lines[i]))
                    
                    self.places[place] = {
                        "owner": user,
                        "art": lines
                    }
                    # when moving, free the old place
                    # art stays, but anyone is free to overwrite it
                    if user in self.owners and place != self.owners[user]:
                        self.places[self.owners[user]]["owner"] = None
                    self.owners[user] = place
        
        except Exception as err:
            print("error occured while loading user "+user+":\n", err)
    
    def toString(self):
        width = 0
        height = 0
        for place in self.places:
            x, y = parsePlace(place)
            if x >= width:
                width = x + 1
            if y >= height:
                height = y + 1
        return \
            "\n".join(
                "".join(
                    self.getCharAtPos(x, y) for x in range(width*self.parcelwidth)
                ) for y in range(height*self.parcelheight)
            )
        
    
    def getCharAtPos(self, x, y):
        place = hashPlace(x//self.parcelwidth, y//self.parcelheight)
        if place in self.places:
            parcel = self.places[place]
            return parcel["art"][y%self.parcelheight][x%self.parcelwidth]
        else:
            return random.choice(backgroundchars)
            
    
    def save(self):
        with open(self.towndatafile, mode='w') as f:
            json.dump({
                "places":self.places,
                "owners":self.owners
                }, f)
    
    def export(self):
        with open(os.path.join(sys.path[0], self.name+".txt"), mode='w') as f:
            f.write(self.toString())
        with open(os.path.join(sys.path[0], self.name+".html"), mode='w') as f:
            f.write(htmlwrapper % html.escape(self.toString()))


def main():
    cadastre = Cadastre("town", "home.txt", 24, 12)
    cadastre.update()
    cadastre.save()
    cadastre.export()

if __name__ == "__main__":
    main()

