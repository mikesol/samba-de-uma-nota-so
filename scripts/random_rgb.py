import random

if __name__ == '__main__':
  for x in range(1):
    print(f'''
    {{
      wedges: pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) +> V.empty,
      ring0: pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) ,
      ring1: pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) ,
      center: pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f}) ,
      background: pt ({(random.random() * 2.0 - 1.0):.4f}) ({(random.random() * 2.0 - 1.0):.4f})
    }}''')

    """
    print(f'''
    +> {{
      wedges: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> V.empty,
      ring0: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 ,
      ring1: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 ,
      center: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 ,
      background: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0
    }}''')
  """
