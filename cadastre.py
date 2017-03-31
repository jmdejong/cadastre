#!/usr/bin/python3


import os
import sys
import json
import random
import re
import string
import html

# set this to "home" in testing environment, and to "/home" when running for real
# todo: find a way to do this automatic
usersdir = "home"

parcelfileprefix = ".cadastre"

backgroundchars = ",,..''``\"" + ' '*100

# backslash which escapes the screen and enters your brain?
regex = '[^0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&\'()*+,-./:;<=>?@[\\\\\\]^_`{|}~ ]'
# this regex is necessary to filter out tabs, newlines and control characters
# if a character would have a different size, it would fuck up all other parcels on that line
replacechars = re.compile(regex)


def hashPlace(x, y):
    return str(x) + "," + str(y)

def parsePlace(place):
    return [int(x) for x in place.split(',')]

def readAsciiImage(f, width, height):
    lines = []
    for i in range(height):
        line = f.readline()
        line = replacechars.sub(' ',line)
        line = line[:width]
        if len(line) < width:
            line += ' '*(width-len(line))
        lines.append(line)
    return lines


class Cadastre:
    
    def __init__(self, name, parcelfile, width, height):
        self.name = name
        self.towndatafile = os.path.join(sys.path[0], name+".json")
        self.parcelwidth = width
        self.parcelheight = height
        self.width = 0
        self.height = 0
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
        # todo: JSON and YAML support
        try:
            with open(parcel) as f:
                # load the desired coordinates
                x, y = [int(x) for x in f.readline().strip().split(' ')]
                place = hashPlace(x, y)
                
                # check if the place is free
                if place not in self.places or self.places[place]["owner"] == None or self.places[place]["owner"] == user:
                    
                    # load the ascii art
                    lines = readAsciiImage(f, self.parcelwidth, self.parcelheight)
                    
                    # if the next line is a minus sign, use the ascii art as linkmap, otherwise read the linkmap
                    mode = f.readline()
                    if mode.strip() == '-':
                        linkmap = lines
                    else:
                        linkmap = readAsciiImage(f, self.parcelwidth, self.parcelheight)
                    
                    # read the mapping of what character corresponds to what link
                    links = {}
                    for line in f:
                        char, url = line.split(' ')
                        links[char] = url
                    
                    self.places[place] = {
                        "owner": user,
                        "art": lines,
                        #"url": url,
                        "linkmap": linkmap,
                        "links": links
                    }
                    # when moving, free the old place
                    # art stays, but anyone is free to overwrite it
                    if user in self.owners and place != self.owners[user]:
                        self.places[self.owners[user]]["owner"] = None
                    self.owners[user] = place
        
        except Exception as err:
            # todo: find a good but non-intrusive way to inform the user that their file is wrong
            print("error occured while loading user "+user+":\n", err)
    
    def updateSize(self):
        # find the maximum locations, so a viewer knows how big it should be
        width = 0
        height = 0
        for place in self.places:
            x, y = parsePlace(place)
            if x >= width:
                width = x + 1
            if y >= height:
                height = y + 1
        self.width = width
        self.height = height
        
    
    def toString(self):
        return \
            "\n".join(
                "".join(
                    self.getCharAtPos(x, y) for x in range(self.width*self.parcelwidth)
                ) for y in range(self.height*self.parcelheight)
            )
    
    
    def toHtml(self):
        # create an html file as repesentation of the map
        # a better way would be to make a viewer in html/js that asks for the json data using AJAX
        # and update its own content, but for now this is easier
        htmlwrapper="""
<!DOCTYPE html>
<html>
<!-- See tilde.town/~troido/cadastre for instructions -->
<head>
    <meta charset='utf-8'>
    <style>
a {text-decoration: none}
    </style>
</head>
<body>
    <pre>%s</pre>
</body>
<!-- Cadastre made by ~Troido; art by tilde.town users -->
</html>"""
        return htmlwrapper % \
            "\n".join(
                "".join(
                    self.getHtmlAtPos(x, y) for x in range(self.width*self.parcelwidth)
                ) for y in range(self.height*self.parcelheight)
            )
    
    
    def getCharAtPos(self, x, y):
        place = hashPlace(x//self.parcelwidth, y//self.parcelheight)
        if place in self.places:
            parcel = self.places[place]
            return parcel['art'][y%self.parcelheight][x%self.parcelwidth]
        else:
            return random.choice(backgroundchars)
    
    def getHtmlAtPos(self, x, y):
        place = hashPlace(x//self.parcelwidth, y//self.parcelheight)
        if place in self.places:
            parcel = self.places[place]
            char = html.escape(parcel['art'][y%self.parcelheight][x%self.parcelwidth])
            linkchar = parcel['linkmap'][y%self.parcelheight][x%self.parcelwidth]
            if linkchar in parcel["links"]:
                url = parcel["links"][linkchar]
                # TODO: escape special characters in url
                return '<a href="%s">%s</a>' % (url, char)
            else:
                return char
        else:
            return random.choice(backgroundchars)
    
    
    def save(self):
        with open(self.towndatafile, mode='w') as f:
            json.dump({
                "places":self.places,
                "owners":self.owners
                }, f)
    
    def export(self):
        self.updateSize()
        with open(os.path.join(sys.path[0], self.name+".txt"), mode='w') as f:
            f.write(self.toString())
        with open(os.path.join(sys.path[0], self.name+".html"), mode='w') as f:
            f.write(self.toHtml())


def main():
    cadastre = Cadastre("town", "home.txt", 24, 12)
    cadastre.update()
    cadastre.save()
    cadastre.export()

if __name__ == "__main__":
    main()

