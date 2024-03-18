import numpy as np
from typing import Union
import time

# CONSTANTS
DATA_LIMIT: int = 50000
GENERATION_TIME_LIMIT: int = 5000
BASE_PROFIT: int = 10000
PROFIT_PER_BYTE: int = 5
PROFIT_PER_SECOND: int = 10
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
  storageFunction: function = _raise(Exception('Storage Function Not Set'))
  generateFunction: function  = _raise(Exception('Generate Function Not Set'))
  # DUNDER METHODS
  def __init__(self) -> None:
    # Why am I using numpy arrays for this?
    # Static Typing.
    # Low Level Memory Management.
    # Numpy will calculate the size of the array without me having to write a function.
    self.data: np.array = np.array([[],[]], dtype=bytes) # its two dimentional because you also need to store the emails.
    self._cache: str = ""
    self.generationTime: int = 1
    
  
  # PROPERTIES
  @property
  def storageUsage(self) -> int:
    return self.data.nbytes + len(self.cache)
  
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
  def runTests(self) -> bool:
    self.storageFunction(Database.generateEntries())
    emails = self.generateFunction()
    if emails.shape[0] != 1000:
      print('Emails not generated correctly')
      return False
    for email in emails:
      if not email[1].endswith('.example.com'):
        print('Emails not generated correctly')
        return False
      # check if email[0] starts with this "Hello, how are you "
      if not email[0].startswith('Hello, how are you '):
        print('Emails not generated correctly')
        return False
    return True
    

  def calculateProfits(self, iterations: int = 10) -> float:
    # should take less than 5000 seconds to make messages
    profitArray = np.array([], dtype=int)
    for _ in range(iterations):
      self.__init__()
      if not self.runTests():
        print('Tests Failed')
        return 0
      generationProfits = (GENERATION_TIME_LIMIT-self.totalMessageGenerationTime)*PROFIT_PER_SECOND if self.totalMessageGenerationTime > GENERATION_TIME_LIMIT else 0
      if generationProfits == 0:
        print('Generation Time too slow:', self.totalMessageGenerationTime)
        return 0
      storageProfits = (DATA_LIMIT-self.storageUsage)*PROFIT_PER_BYTE if self.storageUsage < DATA_LIMIT else 0
      if storageProfits == 0:
        print('Storage Usage too high:', self.storageUsage)
        return 0
      totalProfits = BASE_PROFIT + generationProfits + storageProfits
      profitArray = np.append(profitArray, totalProfits)
    return np.mean(profitArray)
