# Description
DES implementation in Common Lisp.

# Performance
Lisp Version: sbcl-1.1.8-x86-64-darwin

## Clock cycles per byte
<table>
<tr><th></th> <th>2.26 GHz Intel Core 2 Duo</th> <th>2.8 GHz Intel i7</th></tr>
<tr><td>DES </td> <td>-</td> <td>1100</td></tr>
<tr><td>DES3</td> <td>-</td> <td>-</td></tr>
</table>

## Consing
<table>
<tr><th></th> <th>bytes</th></tr>
<tr><td>DES </td> <td>32</td></tr>
<tr><td>DES3</td> <td>-</td></tr>
</table>
