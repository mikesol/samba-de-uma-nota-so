import random

if __name__ == '__main__':
  for x in range(16):
    print(f'''
    +> {{
      wedges: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 +> V.empty,
      ring0: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 ,
      ring1: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 ,
      center: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0 ,
      background: frgba {random.randint(0,255)} {random.randint(0,255)} {random.randint(0,255)} 1.0
    }}''')
