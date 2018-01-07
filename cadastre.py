#!/usr/bin/python3


import os
import sys
import json
import random
import re
import string
import html
import time
import hashlib


# set this to "home" in testing environment, and to "/home" when running for real
# todo: find a way to do this automatic
# solution: just make "home" a symlink to "/home" in the real thing
usersdir = "home"
publicdir = os.path.join(sys.path[0],"public")

parcelfileprefix = ".cadastre"

backgroundchars = ",,..''``\"" + ' '*100

maxwidth = 600
maxheight = 300


# These include: printable ascii characters, unicode range 0xa0 to 0x100 (minus \xad), unicode range 0x2200 to 0x22f0, unicode range 2500 to 2800
regex = '[^0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&\'()*+,-./:;<=>?@[\\\\\\]^_`{|}~ ¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ─━│┃┄┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞┟┠┡┢┣┤┥┦┧┨┩┪┫┬┭┮┯┰┱┲┳┴┵┶┷┸┹┺┻┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬╭╮╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾╿▀▁▂▃▄▅▆▇█▉▊▋▌▍▎▏▐░▒▓▔▕▖▗▘▙▚▛▜▝▞▟■□▢▣▤▥▦▧▨▩▪▫▬▭▮▯▰▱▲△▴▵▶▷▸▹►▻▼▽▾▿◀◁◂◃◄◅◆◇◈◉◊○◌◍◎●◐◑◒◓◔◕◖◗◘◙◚◛◜◝◞◟◠◡◢◣◤◥◦◧◨◩◪◫◬◭◮◯◰◱◲◳◴◵◶◷◸◹◺◻◼◽◾◿☀☁☂☃☄★☆☇☈☉☊☋☌☍☎☏☐☑☒☓☔☕☖☗☘☙☚☛☜☝☞☟☠☡☢☣☤☥☦☧☨☩☪☫☬☭☮☯☰☱☲☳☴☵☶☷☸☹☺☻☼☽☾☿♀♁♂♃♄♅♆♇♈♉♊♋♌♍♎♏♐♑♒♓♔♕♖♗♘♙♚♛♜♝♞♟♠♡♢♣♤♥♦♧♨♩♪♫♬♭♮♯♰♱♲♳♴♵♶♷♸♹♺♻♼♽♾♿⚀⚁⚂⚃⚄⚅⚆⚇⚈⚉⚊⚋⚌⚍⚎⚏⚐⚑⚒⚓⚔⚕⚖⚗⚘⚙⚚⚛⚜⚝⚞⚟⚠⚡⚢⚣⚤⚥⚦⚧⚨⚩⚪⚫⚬⚭⚮⚯⚰⚱⚲⚳⚴⚵⚶⚷⚸⚹⚺⚻⚼⚽⚾⚿⛀⛁⛂⛃⛄⛅⛆⛇⛈⛉⛊⛋⛌⛍⛎⛏⛐⛑⛒⛓⛔⛕⛖⛗⛘⛙⛚⛛⛜⛝⛞⛟⛠⛡⛢⛣⛤⛥⛦⛧⛨⛩⛪⛫⛬⛭⛮⛯⛰⛱⛲⛳⛴⛵⛶⛷⛸⛹⛺⛻⛼⛽⛾⛿✀✁✂✃✄✅✆✇✈✉✊✋✌✍✎✏✐✑✒✓✔✕✖✗✘✙✚✛✜✝✞✟✠✡✢✣✤✥✦✧✨✩✪✫✬✭✮✯✰✱✲✳✴✵✶✷✸✹✺✻✼✽✾✿❀❁❂❃❄❅❆❇❈❉❊❋❌❍❎❏❐❑❒❓❔❕❖❗❘❙❚❛❜❝❞❟❠❡❢❣❤❥❦❧❨❩❪❫❬❭❮❯❰❱❲❳❴❵❶❷❸❹❺❻❼❽❾❿➀➁➂➃➄➅➆➇➈➉➊➋➌➍➎➏➐➑➒➓➔➕➖➗➘➙➚➛➜➝➞➟➠➡➢➣➤➥➦➧➨➩➪➫➬➭➮➯➰➱➲➳➴➵➶➷➸➹➺➻➼➽➾➿⟀⟁⟂⟃⟄⟅⟆⟇⟈⟉⟊⟋⟌⟍⟎⟏⟐⟑⟒⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟢⟣⟤⟥⟦⟧⟨⟩⟪⟫⟬⟭⟮⟯⟰⟱⟲⟳⟴⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿∀∁∂∃∄∅∆∇∈∉∊∋∌∍∎∏∐∑−∓∔∕∖∗∘∙√∛∜∝∞∟∠∡∢∣∤∥∦∧∨∩∪∫∬∭∮∯∰∱∲∳∴∵∶∷∸∹∺∻∼∽∾∿≀≁≂≃≄≅≆≇≈≉≊≋≌≍≎≏≐≑≒≓≔≕≖≗≘≙≚≛≜≝≞≟≠≡≢≣≤≥≦≧≨≩≪≫≬≭≮≯≰≱≲≳≴≵≶≷≸≹≺≻≼≽≾≿⊀⊁⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋⊌⊍⊎⊏⊐⊑⊒⊓⊔⊕⊖⊗⊘⊙⊚⊛⊜⊝⊞⊟⊠⊡⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊲⊳⊴⊵⊶⊷⊸⊹⊺⊻⊼⊽⊾⊿⋀⋁⋂⋃⋄⋅⋆⋇⋈⋉⋊⋋⋌⋍⋎⋏⋐⋑⋒⋓⋔⋕⋖⋗⋘⋙⋚⋛⋜⋝⋞⋟⋠⋡⋢⋣⋤⋥⋦⋧⋨⋩⋪⋫⋬⋭⋮⋯π]'

# this regex is necessary to filter out tabs, newlines and control characters
# if a character would have a different size, it would fuck up all other parcels on that line
replacechars = re.compile(regex)


# return a 32-bit random integer based on the arguments
def intHash(*args):
    return int(hashlib.md5(' '.join(map(str,args)).encode('utf-8')).hexdigest()[:8],16)

def floatHash(*args):
    return intHash(*args)/(1<<32)

def backgroundAt(x, y, seed):
    i = int(floatHash(x, y, seed)*len(backgroundchars))
    return backgroundchars[i]

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
        self.seed = time.time()
        if os.path.exists(self.towndatafile):
            with open(self.towndatafile) as f:
                towndata = json.load(f)
                self.places = towndata["places"]
                self.owners = towndata["owners"]
        else:
            self.places = {}
            self.owners = {}

    def update(self):

        self.loadParcelText("@_admin",os.path.join(sys.path[0], "adminparcel"))
        for user in os.listdir(usersdir):
            parcel = os.path.join(usersdir, user, parcelfileprefix, self.parcelfile)
            self.loadParcelText(user, parcel)
        for filename in os.listdir(publicdir):
            if filename[-5:] == ".prcl":
                self.loadParcelText(None, os.path.join(publicdir, filename))


    def loadParcelText(self, user, parcelName):
        # todo: JSON and YAML support
        if os.path.isfile(parcelName + ".txt"):
            parcel = parcelName + ".txt"
        elif os.path.isfile(parcelName + ".prcl"):
            parcel = parcelName + ".prcl"
        else:
            return False
        try:
            with open(parcel) as f:
                # load the desired coordinates
                x, y = [int(x) for x in f.readline().strip().split(' ')]
                place = hashPlace(x, y)

                # check if the place is free
                if place in self.places and self.places[place]["owner"] != None and self.places[place]["owner"] != user:
                    print("{} tried taking place {} which is taken.".format(user, place))

                # load the ascii art
                lines = readAsciiImage(f, self.parcelwidth, self.parcelheight)

                # if the next line is a minus sign, use the ascii art as linkmask, otherwise read the linkmask
                mode = f.readline()
                if mode.strip() == '-':
                    linkmask = lines
                else:
                    linkmask = readAsciiImage(f, self.parcelwidth, self.parcelheight)

                # read the mapping of what character corresponds to what link
                links = {}
                for line in f:
                    s = line.split(' ')
                    if len(s) == 2:
                        char, url = s
                        links[char] = url

                self.places[place] = {
                    "owner": user,
                    "art": lines,
                    #"url": url,
                    # "linkmap": linkmask,
                    "linkmask": linkmask,
                    "links": links
                }
                # when moving, free the old place
                if user in self.owners and place != self.owners[user]:
                    del self.places[self.owners[user]]
                self.owners[user] = place

                return True

        except Exception as err:
            # todo: find a good but non-intrusive way to inform the user that their file is wrong
            print("error occured while loading user "+user+":\n", err)

    #def loadParcelJSON(self, user, parcel):


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
                    self.getCharAtPos(x, y) for x in range((maxwidth))#, self.width*self.parcelwidth))
                ) for y in range(min(maxheight, maxheight))#self.height*self.parcelheight))
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
</html>
"""
        return htmlwrapper % \
            "\n".join(
                "".join(
                    self.getHtmlAtPos(x, y) for x in range((maxwidth))#, self.width*self.parcelwidth))
                ) for y in range(min(maxheight, maxheight))#self.height*self.parcelheight))
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
            if 'linkmask' in parcel:
                linkchar = parcel['linkmask'][y%self.parcelheight][x%self.parcelwidth]
                if linkchar in parcel["links"]:
                    url = html.escape(parcel["links"][linkchar], True)
                    r = '<a href="%s">%s</a>' % (url, char)
                else:
                    r = char
            else:
                r = char
            if (x%self.parcelwidth) == 0 and (y%self.parcelheight) == 0:
                r = '<span id="{}">'.format(parcel['owner']) + r
            if ((x+1)%self.parcelwidth) == 0 and (y%self.parcelheight) == 0:
                r += '</span>'
        else:
            r = backgroundAt(x, y, self.seed)
        if (x%self.parcelwidth) == 0 and (y%self.parcelheight) == 0:
            r = '<span id="{}"></span>'.format(place) + r
        return r


    def save(self):
        tmpfile = self.towndatafile+".tmpcadastre"
        with open(tmpfile, mode='w') as f:
            json.dump({
                "places":self.places,
                "owners":self.owners,
                "seed": self.seed
                }, f)
        os.rename(tmpfile, self.towndatafile)

    def export(self):
        self.updateSize()
        townString = self.toString()
        with open(os.path.join(sys.path[0], self.name+".txt"), mode='w') as f:
            f.write(townString)
        townHtml = self.toHtml()
        with open(os.path.join(sys.path[0], self.name+".html"), mode='w') as f:
            f.write(townHtml)


def main():
    cadastre = Cadastre("town", "home", 24, 12)
    cadastre.update()
    #print(c)
    cadastre.save()
    cadastre.export()

if __name__ == "__main__":
    main()
