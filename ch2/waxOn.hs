module WaxOnLet where

waxOn = x * 5
    where z = 7
          y = z + 8
          x = y ^ 2

triple x = x * 5

waxOff x = triple x