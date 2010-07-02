package org.cellml.swing

import scala.swing._
import javax.swing.BoxLayout


class ColumnPanel extends FlowPanel {
  override lazy val peer: javax.swing.JPanel = 
    new javax.swing.JPanel() with SuperMixin
  peer.setLayout(new BoxLayout(peer, BoxLayout.Y_AXIS))
}
