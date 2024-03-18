import numpy as np
from typing import Union
import time

# CONSTANTS
STATES: list = [
      'STATE.AK.TXT',
      'STATE.AL.TXT',
      'STATE.AR.TXT',
      'STATE.AZ.TXT',
      'STATE.CA.TXT',
      'STATE.CO.TXT',
      'STATE.CT.TXT',
      'STATE.DC.TXT',
      'STATE.DE.TXT',
      'STATE.FL.TXT',
      'STATE.GA.TXT',
      'STATE.HI.TXT',
      'STATE.IA.TXT',
      'STATE.ID.TXT',
      'STATE.IL.TXT',
      'STATE.IN.TXT',
      'STATE.KS.TXT',
      'STATE.KY.TXT',
      'STATE.LA.TXT',
      'STATE.MA.TXT',
      'STATE.MD.TXT',
      'STATE.ME.TXT',
      'STATE.MI.TXT',
      'STATE.MN.TXT',
      'STATE.MO.TXT',
      'STATE.MS.TXT',
      'STATE.MT.TXT',
      'STATE.NC.TXT',
      'STATE.ND.TXT',
      'STATE.NE.TXT',
      'STATE.NH.TXT',
      'STATE.NJ.TXT',
      'STATE.NM.TXT',
      'STATE.NV.TXT',
      'STATE.NY.TXT',
      'STATE.OH.TXT',
      'STATE.OK.TXT',
      'STATE.OR.TXT',
      'STATE.PA.TXT',
      'STATE.RI.TXT',
      'STATE.SC.TXT',
      'STATE.SD.TXT',
      'STATE.TN.TXT',
      'STATE.TX.TXT',
      'STATE.UT.TXT',
      'STATE.VA.TXT',
      'STATE.VT.TXT',
      'STATE.WA.TXT',
      'STATE.WI.TXT',
      'STATE.WV.TXT',
      'STATE.WY.TXT'
    ]

def _raise(e: Exception) -> None:
  raise e

def lineCount(file: str) -> int:
  with open(f'data/{file}', 'r') as f:
    return len(f.readlines())

class Database:
  # DUNDER METHODS
  def __init__(self) -> None:
    # Why am I using numpy arrays for this?
    # Static Typing.
    # Low Level Memory Management.
    # Numpy will calculate the size of the array without me having to write a function.
    self.data: np.array = np.array([[],[]], dtype=bytes) # its two dimentional because you also need to store the emails.
    self._cache: str = ""
    self.generationTime: int = 1
    self.storageFunction: function = _raise(Exception('Storage Function Not Set'))
    self.generateFunction: function  = _raise(Exception('Generate Function Not Set'))
  
  # PROPERTIES
  @property
  def storageUsage(self) -> int:
    return self.data.nbytes
  
  @property
  def totalMessageGenerationTime(self) -> int:
    return self.generationTime*self.data.shape[1]
  
  @property
  def cache(self) -> Union[str, bytes]:
    return self._cache
  
  @cache.setter
  def cache(self, value: Union[str, bytes]) -> None:
    if len(value) > 8:
      raise ValueError('Cache cannot be longer than 8 bytes')
    self._cache = value

  # STATIC METHODS
  @staticmethod
  def generateEntries() -> np.array:
    startTime = time.time()
    # make an array of arrays of strings, with a length of 51, one for each state
    newList = []
    for _ in range(51):
      newList.append([])
    used = np.array(newList, dtype=int)
    finalEntries = np.array([['Name','Email']], dtype=str)
    while finalEntries.size < 2002:
      print('Size:', finalEntries.size)
      state = np.random.randint(0, 50)
      entry = np.random.randint(0, lineCount(STATES[state]))
      if entry in used[state]:
        print('Duplicate Entry Found. Skipping.')
        continue
      used[state] = np.append(used[state], entry)
      with open(f'data/{STATES[state]}', 'r') as f:
        lines = f.readlines()
        name = lines[entry].strip().split(',')[3]
        email = f'{name}.{state}@{entry}.example.com'
        print(f'Generated: {name}, {email}')
        finalEntries = np.append(finalEntries, [[name, email]] , axis=0)
    # Remove the first row, which is the column names.
    finalEntries = np.delete(finalEntries, 0, axis=0)
    end_time = time.time()
    print(f'Generated 1000 entries in {end_time-startTime} seconds.')
    return finalEntries
  # PUBLIC METHODS