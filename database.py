import numpy as np
from typing import Union
import time

# CONSTANTS
DATA_LIMIT: int = 108000 # Value doubled to account for extra row
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

def _default() -> None:
  raise Exception('Function Not Set')

def lineCount(file: str) -> int:
  with open(f'data/{file}', 'r') as f:
    return len(f.readlines())

class Database:
  storageFunction = _default
  generateFunction = _default
  # DUNDER METHODS
  def __init__(self) -> None:
    # Why am I using numpy arrays for this?
    # Static Typing.
    # Low Level Memory Management.
    # Numpy will calculate the size of the array without me having to write a function.
    self._data: list = []
    self._cache: str = ""
    self.generationTime: int = 1
    
  
  # PROPERTIES
  @property
  def storageUsage(self) -> int:
    return self.data.nbytes + len(self.cache)
  
  @property
  def totalMessageGenerationTime(self) -> int:
    return self.generationTime*self.data.shape[0]
  
  @property
  def cache(self) -> Union[str, bytes]:
    return self._cache
  
  @cache.setter
  def cache(self, value: Union[str, bytes]) -> None:
    if len(value) > 8:
      raise ValueError('Cache cannot be longer than 8 bytes')
    self._cache = value

  @property
  def data(self) -> list:
    return np.array(self._data)
  
  @data.setter
  def data(self, value: list) -> None:
    self._data = value

  # STATIC METHODS
  @staticmethod
  def generateEntries() -> np.array:
    startTime = time.time()
    # make an array of arrays of strings, with a length of 51, one for each state
    used = [set() for _ in range(51)]
    finalEntries = [['Name','Email']]
    # Read all the lines from the files
    # Its wild how much faster it is when you do it this way rather than reading the file each time.
    lines = [open(f'data/{state}', 'r').readlines() for state in STATES]
    while len(finalEntries) < 1001:
      state = np.random.randint(0, 50)
      entry = np.random.randint(0, len(lines[state]))
      if entry in used[state]:
        continue
      used[state].add(entry)
      name = lines[state][entry].strip().split(',')[3]
      email = f'{name}.{state}@{entry}.example.com'
      finalEntries.append([name, email])
    # Remove the first row, which is the column names.
    finalEntries.pop(0)
    end_time = time.time()
    print(f'Generated 1000 entries in {end_time-startTime} seconds.')
    return np.array(finalEntries, dtype=str)
  
  # PUBLIC METHODS
  def runTests(self) -> bool:
    self.storageFunction(self, Database.generateEntries())
    print(len(self.generateFunction(self)))
    emails = np.array(self.generateFunction(self))
    # make sure there are 1000 rows
    if emails.shape[0] != 1000:
      print('Shape is not of length 1000:', emails.shape[0])
      return False
    for email in emails:
      if not email[1].endswith(b'.example.com'):
        print(email[1])
        print('Email does not end with .example.com')
        return False
      # check if email[0] starts with this "Hello, how are you "
      if not email[0].startswith(b'Hello, how are you '):
        print('Email Messaeg does not start with "Hello, how are you "')
        return False
    return True
    

  def calculateProfits(self, iterations: int = 10) -> float:
    profitArray = np.array([], dtype=int)
    for i in range(iterations):
      print('Begining Iteration:', _+1)
      self.__init__()
      if not self.runTests():
        print(f'\n----- Iteration {i+1}: FAILED -----')
        print('Final Data:,', self.data if self.data else '{Cache Empty}')
        print('Final Data Lenght:', len(self.data), 'Entries')
        print('Final Cache:', self.cache)
        print('Storage Usage:', self.storageUsage, 'Bytes')
        print('Total Message Generation Time:', self.totalMessageGenerationTime, 'Seconds')
        return 0
      generationProfits = (GENERATION_TIME_LIMIT-self.totalMessageGenerationTime)*PROFIT_PER_SECOND if self.totalMessageGenerationTime < GENERATION_TIME_LIMIT else 0
      if generationProfits == 0:
        print('Generation Time too slow:', self.totalMessageGenerationTime)
        return 0
      storageProfits = (DATA_LIMIT-self.storageUsage)*PROFIT_PER_BYTE if self.storageUsage < DATA_LIMIT else 0
      if storageProfits == 0:
        print('Storage Usage too high:', self.storageUsage)
        return 0
      totalProfits = BASE_PROFIT + generationProfits + storageProfits
      print(f'\n----- Iteration {i+1}: PASSED -----')
      print('Final Data:,', self.data if self.data else '{Cache Empty}')
      print('Final Data Lenght:', len(self.data), 'Entries')
      print('Final Cache:', self.cache)
      print('Storage Usage:', self.storageUsage, 'Bytes')
      print('Total Message Generation Time:', self.totalMessageGenerationTime, 'Seconds')
      print('Generation Profits: $', generationProfits/100)
      print('Storage Profits: $', storageProfits/100)
      print('Total Profits: $', totalProfits/100)
      profitArray = np.append(profitArray, totalProfits)
    print('\nAll Iterations Run')
    print('Average Calculated Profits: $', np.mean(profitArray)/100)
    return np.mean(profitArray)/100
