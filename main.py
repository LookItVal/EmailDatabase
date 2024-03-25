from database import Database
import numpy as np

def example1(i: int = 10):
  # This initializes the database object.
  db = Database()
  
  # This is the function that will take the data and store it in the database.
  def store(self: Database, entries):
    data = []
    for row in entries:
      newRow = ['Hello, how are you ', row[0], row[1]]
      data.append(newRow)
    self.data = np.array(data, dtype="S35")

  # This is the function that will generate the emails from the data in the database.
  def generate(self: Database):
    emails = []
    for row in self.data:
      emails.append([row[0]+row[1], row[2]])
    return emails

  # This will link everything up with our database object.
  db.storageFunction = store.__get__(db, Database)
  db.generateFunction = generate.__get__(db, Database)
  # Here you will decide based on your generate algorithm how long it should take to generate the emails.
  db.generationTime = 1
  
  # This will run the tests to make sure everything is working correctly.
  # By default it will generate 10 iterations of 1000 emails with different combinations of names for the entries.
  # If any one test fails, it will return with a profit of 0.
  # If all tests pass, it will return with the average calculated profit for each iteration of the tests.
  db.calculateProfits(i)


def example2(i: int = 10):
  db = Database()

  # This is only different in that it uses a different data type for the data.
  def store(self: Database, entries):
    data_types = [('Email Content', 'S19'), ('Name', 'S30'), ('Email', 'S35')]
    data = []
    for row in entries:
      newRow = ('Hello, how are you ', row[0], row[1])
      data.append(newRow)
    self.data = np.array(data, dtype=data_types)

  def generate(self: Database):
    emails = []
    for row in self.data:
      emails.append([row[0]+row[1], row[2]])
    return emails
  
  db.storageFunction = store.__get__(db, Database)
  db.generateFunction = generate.__get__(db, Database)
  db.generationTime = 1

  db.calculateProfits(i)


def cacheingExample(i: int = 10):
  db = Database()

  # This is only different in that it stores a cache value with some of the email conetent in it.
  def store(self: Database, entries):
    data_types = [('Email Content', 'S11'), ('Name', 'S30'), ('Email', 'S35')]
    data = []
    self.cache = 'Hello, h'
    for row in entries:
      newRow = ('ow are you ', row[0], row[1])
      data.append(newRow)
    self.data = np.array(data, dtype=data_types)

  def generate(self: Database):
    emails = []
    cache = self.cache
    for row in self.data:
      emails.append([cache + row[0].decode() + row[1].decode(), row[2].decode()])
    return emails
  
  db.storageFunction = store.__get__(db, Database)
  db.generateFunction = generate.__get__(db, Database)
  db.generationTime = 1

  db.calculateProfits(i)

#def duplicationExample(i):
#  db = Database()
#
#  # This is only different in that it doesnt stores all duplicate names as the same variable.
#  def store(self: Database, entries):
#    data_types = [('Email Content', 'S19'), ('Name', 'S30'), ('Emails', 'S35')]
#    data = []
#    for row in entries:
#      newRow = ('Hello, how are you ', row[0], row[1])
#      data.append(newRow)
#    self.data = np.array(data, dtype=data_types)
#
#  def generate(self: Database):
#    emails = []
#    for row in self.data:
#      emails.append([row[0]+row[1], row[2]])
#    return emails
#  
#  db.storageFunction = store.__get__(db, Database)
#  db.generateFunction = generate.__get__(db, Database)
#  db.generationTime = 1
#
#  db.calculateProfits(i)


def compressionExample():
  db = Database()

  # This is only different in that it stored data in a custom compressed format.
  def compress(string: str) -> bytes:
    key = ' abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,'
    compressed_string = []
    for char in string:
      if char not in key:
        raise Exception('Invalid character in string')
      compressed_string.append(np.unpackbits([np.uint8(key.index(char))]))
    compressed_string = np.array(compressed_string, dtype='uint8')
    final_compressed = np.unpackbits(np.array([],dtype='uint8'))
    for bitarray in compressed_string:
      final_compressed = np.append(final_compressed, bitarray[2:])
    return np.packbits(final_compressed)


  def decompress(compressed: np.ndarray) -> str:
    key = ' abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,'
    compressed = np.unpackbits(compressed)
    new_compressed = np.unpackbits(np.array([],dtype='uint8'))
    for i in range(0, len(compressed), 6):
      new_compressed = np.append(np.zeros(2), compressed[i:i+6])
    print(new_compressed)
    decompressed = np.packbits(new_compressed)
    return ''.join([key[int(byte)] for byte in decompressed])
  # THESE DONT WORK YET



  def store(self: Database, entries):
    data_types = [('Email Content', 'S19'), ('Name', 'S30'), ('Email', 'S35')]
    data = []
    for row in entries:
      newRow = ('Hello, how are you ', row[0], row[1])
      data.append(newRow)
    self.data = np.array(data, dtype=data_types)

  def generate(self: Database):
    emails = []
    for row in self.data:
      emails.append([row[0]+row[1], row[2]])
    return emails
  
  db.storageFunction = store.__get__(db, Database)
  db.generateFunction = generate.__get__(db, Database)
  db.generationTime = 1

  db.calculateProfits(i)
def combinedExample():
  print('This is a combined example')


# This is just basic python boilerplate below.
def main():
  example2(1)
  print('----------------')
  duplicationExample(10)


if __name__ == "__main__":
  main()
