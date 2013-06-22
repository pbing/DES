# Description
DES implementation in Common Lisp.
Written for readability with almost no performance optimizations.

# Performance
Lisp Version: sbcl-1.1.8-x86-64-darwin

## Clock cycles per byte
<table>
<tr><th></th> <th>2.26 GHz Intel Core 2 Duo</th> <th>2.8 GHz Intel i7</th></tr>
<tr><td>DES  </td> <td>-</td> <td>4800</td></tr>
<tr><td>DES-3</td> <td>-</td> <td>8700</td></tr>
</table>

## Consing
No consing.
