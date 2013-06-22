# Description
DES implementation in Common Lisp.

# Performance
Lisp Version: sbcl-1.1.8-x86-64-darwin

## Clock cycles per byte
<table>
<tr><th></th> <th>2.26 GHz Intel Core 2 Duo</th> <th>2.8 GHz Intel i7</th></tr>
<tr><td>des:encrypt-1 </td> <td>4100</td> <td>-</td></tr>
<tr><td>des:encrypt3-1</td> <td>7400</td> <td>-</td></tr>
<tr><td>des:encrypt   </td> <td>6800 </td> <td>4800</td></tr>
<tr><td>des:encrypt3  </td> <td>13300</td> <td>8700</td></tr>
</table>

## Consing
No consing.
