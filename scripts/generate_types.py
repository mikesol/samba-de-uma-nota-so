a={
  "chiffyE1":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/chiffyE1.mp3",
  "chiffyE5":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/chiffyE5.mp3",
  "digistensuE5":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/digistensuE5.mp3",
  "mtDillJabsE4":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/mtDillJabsE4.mp3",
  "shamisenE5":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/shamisenE5.mp3",
  "tinefullE2":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/tinefullE2.mp3",
  "vsMagicE1":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/vsMagicE1.mp3",
  "vsMagicE5":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/vsMagicE5.mp3",
  "chiffyE2":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/chiffyE2.mp3",
  "digistensuE2":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/digistensuE2.mp3",
  "digistensuE6":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/digistensuE6.mp3",
  "mtDillJabsE5":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/mtDillJabsE5.mp3",
  "shamisenE6":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/shamisenE6.mp3",
  "tinefullE3":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/tinefullE3.mp3",
  "vsMagicE2":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/vsMagicE2.mp3",
  "chiffyE3":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/chiffyE3.mp3",
  "digistensuE3":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/digistensuE3.mp3",
  "jarbleIctusE3":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/jarbleIctusE3.mp3",
  "shamisenE3":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/shamisenE3.mp3",
  "tinefullE0":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/tinefullE0.mp3",
  "tinefullE4":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/tinefullE4.mp3",
  "vsMagicE3":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/vsMagicE3.mp3",
  "chiffyE4":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/chiffyE4.mp3",
  "digistensuE4":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/digistensuE4.mp3",
  "mtDillJabsE3":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/mtDillJabsE3.mp3",
  "shamisenE4":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/shamisenE4.mp3",
  "tinefullE1":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/tinefullE1.mp3",
  "tinefullE5":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/tinefullE5.mp3",
  "vsMagicE4":
  "  ""https://klank-share.s3-eu-west-1.amazonaws.com/samba-de-uma-nota-so/vsMagicE4.mp3"
}

createT=lambda x,y: f"""type {x} r
  = ( {y}Gain :: TGain /\ {{ {y}Pan :: Unit }}
    , {y}Pan :: TStereoPanner /\ {{ {y}HPF :: Unit }}
    , {y}HFP :: THighpass /\ {{ {y}Buf :: Unit }}
    , {y}Buf :: TLoopBuf /\ {{}}
    | r
    )"""

create=lambda x: f"""{x}Gain: gain 0.0 {{ {x}Pan: pan 0.0 {{ {x}HFP: highpass 20.0 {{ {x}Buf: loopBuf {{ playbackRate: 1.0 }} "{x}" }} }} }}, """

for x in a.keys():
  # print(createT(x[0].upper()+x[1:],x))
  print(create(x))
  pass

#print(' + '.join([x[0].upper()+x[1:] for x in a.keys()]))
#print(' , '.join(['%s :: Unit' % x for x in a.keys()]))