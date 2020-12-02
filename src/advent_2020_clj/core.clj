(ns advent-2020-clj.core)

(def day-one-input [1732
                    1972
                    1822
                    1920
                    1847
                    1718
                    1827
                    1973
                    1936
                    1865
                    1817
                    1954
                    1939
                    1979
                    1846
                    1989
                    1818
                    398
                    1786
                    1900
                    1949
                    1161
                    609
                    1967
                    1845
                    1795
                    1874
                    1982
                    2010
                    1494
                    1752
                    1803
                    1908
                    1876
                    1977
                    1999
                    1858
                    1885
                    1975
                    1878
                    1784
                    1787
                    1765
                    1778
                    1893
                    1746
                    1807
                    1966
                    1991
                    1905
                    1970
                    1942
                    1792
                    1750
                    713
                    1871
                    1860
                    1931
                    1976
                    1771
                    128
                    390
                    2006
                    1801
                    1946
                    1914
                    1833
                    1515
                    1958
                    1737
                    1887
                    1962
                    1895
                    2004
                    1747
                    1841
                    1793
                    1948
                    1790
                    1808
                    1957
                    1770
                    1960
                    1952
                    1932
                    1782
                    1762
                    1898
                    1919
                    1909
                    1929
                    1964
                    1848
                    1959
                    1381
                    280
                    1899
                    1855
                    1849
                    1889
                    1772
                    1843
                    1767
                    1830
                    1838
                    1869
                    1926
                    1768
                    1789
                    1791
                    1888
                    1371
                    2001
                    1943
                    1741
                    1904
                    1468
                    1969
                    1910
                    649
                    1953
                    1916
                    1852
                    1996
                    1842
                    1950
                    1850
                    1998
                    1963
                    1780
                    1883
                    1955
                    443
                    1773
                    1896
                    1985
                    1809
                    2007
                    1819
                    1891
                    1853
                    1802
                    1861
                    1813
                    1831
                    1974
                    1915
                    1997
                    2000
                    1945
                    1832
                    1763
                    1981
                    1922
                    1862
                    1944
                    1925
                    1742
                    1744
                    1994
                    1961
                    1881
                    1937
                    1911
                    1788
                    1971
                    1890
                    1734
                    1781
                    1984
                    1912
                    1834
                    1766
                    1769
                    1797
                    195
                    1965
                    1934
                    1894
                    1928
                    1759
                    1812
                    1758
                    1988
                    1821
                    1776
                    2009
                    1749
                    1857
                    1785
                    1824
                    1796
                    1930
                    1777
                    1886
                    477
                    1761
                    1800
                    1745
                    1993])
(def test-input [1 2 3 4 5])

(defn pair-adds-to-2020 [p]
  (= 2020 (reduce + p)))

(defn first-pairs [[head & rest :as s]]
  (for [el rest]
    [head el]))

(defn all-pairs [s]
  (loop [vals s
         acc []]
    (prn vals)
    (if (not-empty (rest vals))
      (recur (rest vals) (into acc (first-pairs vals)))
      acc)))

(defn magic-pair [input]
  (->> input
       all-pairs
       (filter pair-adds-to-2020)
       first))

(defn answer [input]
  (->> input
       magic-pair
       (reduce *)))




