//
// Created by nmelzer on 22.11.15.
//

#ifndef PWB_13_STORAGE_H
#define PWB_13_STORAGE_H


#include <vector>

class Storage {
public:
  /** Creates a new Storage from the given vector */
  Storage(const std::vector<int> initial);

  /** Consumes a certain amount of something */
  Storage& consume(int id, int amount);
  Storage& consume(std::vector<int> ids, std::vector<int> amounts);

private:
  std::vector<int> storage_;
};


#endif //PWB_13_STORAGE_H
