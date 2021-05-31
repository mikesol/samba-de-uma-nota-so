import csv
import random

def getLowerBound(n, l, x, y):
  if 'START' in l[x][y][0] or x == 0:
    return x, 0.0
  if n in l[x][y][0]:
    return x, 1.0
  return getLowerBound(n,l,x-1,y)

def getUpperBound(starting, n, l, x, y):
  if 'END' in l[x][y][0] or x == len(l) - 1:
    return x, 0.0
  if n in l[x][y][0] and not starting:
    return x, 1.0
  return getUpperBound(False,n,l,x+1,y)

def getBounds_(n, l, x, y):
  x0, y0 = getLowerBound(n, l, x, y)
  x1, y1 = getUpperBound(True, n, l, x, y)
  return n, x0, y0, x1, y1

def getKBounds(l, x, y):
  return getBounds_('PEAK', l, x, y)

def getDBounds(l, x, y):
  return getBounds_('DISS', l, x, y)

def getPBounds(l, x, y):
  return getBounds_('PAN', l, x, y)

def getFBounds(l, x, y):
  return getBounds_('FILT', l, x, y)

def wb(ll, lh, hl, hh, v, r0, r1, growing):
  v0 = (lh - ll) * r0 + ll
  v1 = (hh - hl) * r1 + hl
  if growing:
    v[2] = v0
    v[4] = v1
  else:
    v[2] = v1
    v[4] = v0

with open('./data/samba-introChiffy.csv') as csvfile:
  reader = csv.reader(csvfile)
  l = [[y for y in x] for x in reader]
  # read
  for x in range(len(l)):
    for y in range(len(l[x])):
      v = l[x][y]
      if (v == 'o' or v == 'x') and (x == 0 or l[x-1][y] == 'n'):
        l[x][y] += 'START'
      if (v == 'o' or v == 'x') and (x == len(l) - 1 or l[x+1][y] == 'n'):
        l[x][y] += 'END'
  # build graph
  for x in range(len(l)):
    for y in range(len(l[x])):
      v = l[x][y]
      if 'START' in v:
        for z in range(x+1,len(l)):
          vv = l[z][y]
          if 'END' in vv:
            peak = random.randint(x+1,z-1)
            l[peak][y] += 'PEAK'
            pan = random.randint(x+1,z-1)
            l[pan][y] += 'PAN'
            filt = random.randint(x+1,z-1)
            l[filt][y] += 'FILT'
            break
        if v[0] == 'o':
          for z in range(x+1,len(l)):
            vv = l[z][y]
            if 'END' in vv:
              peak = random.randint(x+1,z-1)
              l[peak][y] += 'DISS'
              break
  # make tuple
  for x in range(len(l)):
    for y in range(len(l[x])):
      l[x][y] = (l[x][y],)
  # attach bounds
  for x in range(len(l)):
    for y in range(len(l[x])):
      v = l[x][y]
      if v[0] == 'n':
        l[x][y] = ('n', 0.0)
      elif 'x' == v[0][0]:
        t0, x0, y0, x1, y1 = getKBounds(l, x, y)
        t1, x2, y2, x3, y3 = getFBounds(l, x, y)
        t2, x4, y4, x5, y5 = getPBounds(l, x, y)
        l[x][y] = (v[0], (t0,x0,y0,x1,y1), (t1,x2,y2,x3,y3), (t2,x4,y4,x5,y5))
        #print(l[x][y])
      elif 'o' == v[0][0]:
        t0, x0, y0, x1, y1 = getKBounds(l, x, y)
        t1, x2, y2, x3, y3 = getFBounds(l, x, y)
        t2, x4, y4, x5, y5 = getPBounds(l, x, y)
        t3, x6, y6, x7, y7 = getDBounds(l, x, y)
        l[x][y] = (v[0], (t0,x0,y0,x1,y1), (t1,x2,y2,x3,y3), (t2,x4,y4,x5,y5), (t3,x6,y6,x7,y7))
  # mod values
  for x in range(len(l)):
    for y in range(len(l[x])):
      v = l[x][y]
      if v[0][0] == 'o' or v[0][0] == 'x':
        v = list(v)
        for z in range(1, len(v)):
          asL = list(v[z])
          st = asL[1]
          ed = asL[3]
          growing = asL[2] == 0.0
          random.seed((y << 16) | (st << 8) | ed)
          if asL[0] == 'FILT':
            wb(200.0, 2000.0, 200.0, 2000.0, asL, random.random(), random.random(), growing)
          elif asL[0] == 'DISS':
            wb(0.0, 0.0, 0.03, 0.06, asL, random.random(), random.random(), growing)
          elif asL[0] == 'PEAK':
            wb(0.0, 0.0, 0.8, 0.95, asL, random.random(), random.random(), growing)
          elif asL[0] == 'PAN':
            wb(-0.8, -0.3, 0.3, 0.8, asL, random.random(), random.random(), growing)
            if random.random() > 0.5:
              asL[2] *= -1.0
              asL[4] *= -1.0
          v[z] = tuple(asL)
        l[x][y] = tuple(v)
  for x in l:
    print(x)
