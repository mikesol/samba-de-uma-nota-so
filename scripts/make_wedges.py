import random

WEDGES = [
  dict(tx=random.random() - 0.5, ty=random.random() - 0.5, r=random.random() * 0.2 - 0.1, w=x)
  for x in range(12)
]
random.shuffle(WEDGES)

SCHEDULE = [[(y, (1.0 + x - y) / (14.0 - y)) for y in range(min(12,x+1))] for x in range(14)]

def sc(x):
  z = x ** 0.2
  return 0.0 if z < 0.001 else z

def rsc(x):
  z = 1.0 - sc(x)
  return 0.0 if z < 0.001 else z

NO_ROTATION=0.0

# rotate (pi * {(rsc(x[1]) * WEDGES[i]["r"] * NO_ROTATION):.3f}) $ 
def make_wedge(n):
  v = f' <> '.join(
    [f'(translate (width * {(rsc(x[1]) * WEDGES[i]["tx"]):.3f}) (height * {(rsc(x[1]) * WEDGES[i]["ty"]):.3f}) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * {(WEDGES[i]["w"]-1.0)/6.0 }) (pi * {WEDGES[i]["w"]/6.0}) ({sc(x[1]):.3f} * mwh * 0.45))))' for i, x in enumerate(SCHEDULE[n])
    ]
    )
  return f'''
wedge{n} :: DOMRect -> Painting
wedge{n} {{ width, height }} = {v}
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height
'''

if __name__ == '__main__':
  #print(WEDGES)
  #print(SCHEDULE)
  for x in range(14):
    print(make_wedge(x))