#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector majority_rule_R2022A(Rcpp::NumericVector x, int value_of_interest, size_t ni, size_t nw) {
  
  // initialise output as empty vector of length ni (= number of cells in the original raster)
  Rcpp::NumericVector out(ni);
  
  // initialise start index
  size_t start = 0;
  
  // loop over the cells of the initial raser
  for (size_t i=0; i<ni; i++) {
    
    // compute the end index of window around cell i (with nw = length of window)
    size_t end = start + nw;
    
    // get value of focal cell (index: start + 4)
    int focalcell = x[start+4];
    
    // if the focal cell is NAN or not the value of interest: do nothing
    if (std::isnan(focalcell) || (focalcell != value_of_interest)) {
      out[i] = Rcpp::NumericVector::get_na();
      
    } else {
      
      // calculate which ID is most frequent in the window with Boyer-Moore Majority Voting Algorithm
      // for more details on the algorithm see: https://www.geeksforgeeks.org/boyer-moore-majority-voting-algorithm/
      
      // the Boyer-Moore Majority Voting Algorithm searches for the majority element in a list, 
      // given that is should have more than N/2 occurrences (with N the number of elements in the list)
      
      // STEP 1: find candidate element and start with votes = 1
      int votes = 0;
      size_t candidate_index = -1;
      
      // Loop through the list and apply the algorithm
      for (size_t j = start; j < end; j++) {
        if(votes == 0) {
          candidate_index = j;
          votes = 1;
        } else {
          if(x[j] == x[candidate_index]) {
            votes++;
          } else {
            votes--;
          }
        }
      }
      
      // count the number of occurrences of the candidate element
      int count = 0;
      for (size_t j = start; j < end; j++) {
        if (x[j] == x[candidate_index]) {
          count++;
        }
      }
      
      // the candidate element cannot be the value of interest (= adjacent cell, not urban centre) AND
      // should have 5 or more occurrences
      if ((x[candidate_index] != value_of_interest) && (count >= 5)){
        out[i] = x[candidate_index];
        
      // else: do nothing
      } else {
        out[i] = Rcpp::NumericVector::get_na();
      }
    }
    start = end;
  }
  return out;
}


// [[Rcpp::export]]
Rcpp::NumericVector majority_rule_R2023A(Rcpp::NumericVector x, int adj_value, int ignore_value, int water_value, int adj_ignore, size_t ni, size_t nw) {
  
  // initialise output as empty vector of length ni (= number of cells in the original raster)
  Rcpp::NumericVector out(ni);
  
  // initialise start index
  size_t start = 0;
  
  // loop over the cells of the initial raster
  for (size_t i=0; i<ni; i++) {
    
    // compute the end index of window around cell i (with nw = length of window)
    size_t end = start + nw;
    
    // get value of focal cell (index: start + 4)
    int focalcell = x[start+4];
    
    // if the focal cell is NAN or not adjacent to an urban centre: do nothing
    if (std::isnan(focalcell) || 
        ((focalcell != adj_value) && (focalcell != adj_ignore))) {
      out[i] = Rcpp::NumericVector::get_na();
      
    } else {
      
      // search the most frequent element among the not-ignore, not-water neighbors
      
      // initialise variables
      int maxcount = 0;               // max frequency of an element
      bool candidate_found = false;   // whether a candidate is found
      size_t candidate_index;         // the index of the most frequent element (default: none)
      int N = 0;                      // number of valid neighbors
      
      // loop over the neighbours of the cell
      for (size_t j = start; j < end; j++) {
        
        // count the number of occurrences
        int count = 0;
        
        // If the neighbour is valid (the focal cell, water cells and ignore cells are not valid neighbors)
        if ((j != start+4) && (x[j] != ignore_value) && (x[j] != water_value) 
              && (x[j] != adj_ignore)){
          N++;
          
          // Majority element cannot be NA or the adjacent value.
          if (!std::isnan(x[j]) && (x[j] != adj_value)){
            
            // count the number of occurrences
            for (size_t k = start; k < end; k++) {
              if ((k != start+4) && (x[j] == x[k]))
                count++;
            }
            
            // if the number of occurrences is larger than maxcount: replace maxcount
            if (count > maxcount) {
              maxcount = count;
              candidate_index = j;
              candidate_found = true;
            }
          }
        }
      }
      
      // if a candidate is found and it is frequent enough: replace value by candidate element
      if ((candidate_found) &&
          (maxcount*2 > N)) {
        out[i] = x[candidate_index];
        
      // else: do nothing
      } else {
        out[i] = Rcpp::NumericVector::get_na();
      }
    }
    start = end;
  }
  return out;
}
