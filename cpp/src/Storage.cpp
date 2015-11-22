//
// Created by nmelzer on 22.11.15.
//

#include "Storage.h"

Storage::Storage(const std::vector<int> initial): storage_(initial) {}

Storage& Storage::consume(int id, int amount) {
  this->storage_[id] -= amount;
  return this;
}

Storage& Storage::consume(std::vector<int> ids, std::vector<int> amounts) {
  Storage result = new Storage(this->storage_);
  for(int i = 0; i < ids.size(); ++i) {
    result.consume(id[i], amounts[i]);
  }
  return result;
}