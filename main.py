from database import Database
import numpy as np

def main():
  example();

def example():
  db = Database()
  db.storageFunction = exampleStorage
  db.generateFunction = exampleGenerate
  db.calculateProfits()

def exampleStorage(self: Database, entries):
  data = []
  for row in entries:
    newRow = [f'Hello, how are you ', row[0], row[1]]
    data.append(newRow)
  self.data = np.array(data, dtype="S35")

def exampleGenerate(self: Database):
  emails = []
  for row in self.data:
    emails.append([row[0]+row[1], row[2]])
  return emails

if __name__ == "__main__":
  main()
